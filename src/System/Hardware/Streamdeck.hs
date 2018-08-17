{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Streamdeck ( Deck ( serialNumber
                                         , firmware
                                         )
                                  , solidRGB
                                  , openStreamDeck
                                  , enumerateStreamDecks
                                  , setBrightness
                                  , updateDeck
                                  , readButtonState
                                  , writeImage
                                  , sendRaw
                                  ) where

import qualified Data.Bits             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.Word             as DW   (Word16, Word8)
import qualified System.HIDAPI         as HID

import Prelude

newtype ActiveMap = ActiveMap [Bool]

newtype Page = Page (Row, Row, Row)

newtype Row = Row (Image, Image, Image, Image, Image)

type Image = BS.ByteString

type PixelR = DW.Word8
type PixelG = DW.Word8
type PixelB = DW.Word8

data Deck = Deck { _ref         :: HID.Device
                 , serialNumber :: BS.ByteString
                 , firmware     :: BS.ByteString
                 , display      :: Page
                 }

vendorID :: DW.Word16
vendorID  = 0x0fd9

productID :: DW.Word16
productID = 0x0060

packetSize :: Int
packetSize = 4096

page1Pixels :: Int
page1Pixels = 2583

page2Pixels :: Int
page2Pixels = 2601

buttonPixels :: Int
buttonPixels = page1Pixels + page2Pixels

solidRGB :: PixelR -> PixelG -> PixelB -> Image
solidRGB r g b = BS.pack $ take (3 * (buttonPixels - 1)) $ cycle [b, g, r]

defaultPage :: Page
defaultPage = Page ( Row ( solidRGB 255 0 0
                         , solidRGB 204 0 0
                         , solidRGB 153 0 0
                         , solidRGB 102 0 0
                         , solidRGB  51 0 0
                         )
                   , Row ( solidRGB 0 255 0
                         , solidRGB 0 204 0
                         , solidRGB 0 153 0
                         , solidRGB 0 102 0
                         , solidRGB 0  51 0
                         )
                   , Row ( solidRGB 0 0 255
                         , solidRGB 0 0 204
                         , solidRGB 0 0 153
                         , solidRGB 0 0 102
                         , solidRGB 0 0  51
                         )
                   )

setBrightness :: DW.Word8 -> BS.ByteString
setBrightness b
    | b <= 100 = setBrightness' b
    | otherwise = setBrightness' 100

setBrightness' :: DW.Word8 -> BS.ByteString
setBrightness' b = BS.pack [ 0x05                   -- Report 0x05
                           , 0x55, 0xAA, 0xD1, 0x01 -- Command (brightness)
                           ,    b, 0x00, 0x00, 0x00 -- brightness
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           ]

sendRaw :: Deck -> BS.ByteString -> IO ()
sendRaw deck bs =
    if BS.length bs > packetSize then
        (do _ <- HID.write (_ref deck) $ BS.take packetSize bs
            sendRaw deck $ fixContinuationPacket bs)
    else
        (do _ <- HID.write (_ref deck) bs
            return ())

-- In cases where the first byte of a continuation packet is unset, the byte is
-- discarded, resulting in discoloration
fixContinuationPacket :: BS.ByteString -> BS.ByteString
fixContinuationPacket b
    | BS.length b < packetSize = BS.pack []
    | otherwise =
        let rest = BS.drop packetSize b
            byte = BS.head rest
        in if byte > 0 then rest
                       else BS.cons ((B..|.) 1 byte) (BS.drop 1 rest)

writePage :: Deck -> Int -> DW.Word8 -> BS.ByteString -> IO ()
writePage deck p i bs = sendRaw deck $ BS.append (page p i) bs

page :: Int -> DW.Word8 -> BS.ByteString
page 1 i = BS.pack [ 0x02, 0x01, 0x01, 0x00, 0x00,  i+1, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                   , 0x42, 0x4d, 0xf6, 0x3c, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x28, 0x00
                   , 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x48, 0x00
                   , 0x00, 0x00, 0x01, 0x00, 0x18, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0xc0, 0x3c, 0x00, 0x00, 0xc4, 0x0e
                   , 0x00, 0x00, 0xc4, 0x0e, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page 2 i = BS.pack [ 0x02, 0x01, 0x02, 0x00, 0x01,  i+1, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page _ _ = BS.pack []

read :: Deck -> Int -> IO BS.ByteString
read deck = HID.read (_ref deck)

-- Stream Deck reports button state ONLY upon button press/release
-- Stream Deck will send a 16 byte message, with the following format:
-- 01 AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO
-- * Byte 0 being set to 0x01 is static, indicating a "button event" message.
-- * AA-OO are 1 byte, if the low bit is set, the button is pressed.  Bits 1-7
--   appear to be unused.
readButtonState :: Deck -> IO ActiveMap
readButtonState deck =
    bytesToActiveMap . BS.unpack <$> System.Hardware.Streamdeck.read deck 16

bytesToActiveMap :: [DW.Word8] -> ActiveMap
bytesToActiveMap xs
    | length xs /= 16 = emptyActiveMap
    | otherwise = ActiveMap $ map (== 1) $ drop 1 xs

emptyActiveMap :: ActiveMap
emptyActiveMap = ActiveMap $ replicate 15 False

writeImage :: Deck -> DW.Word8 -> Image -> IO ()
writeImage deck button img =
    let page1 = BS.take (3 * page1Pixels) img
        page2 = BS.take (3 * page2Pixels) $ BS.drop (3 * page1Pixels) img
    in do
        writePage deck 1 button page1
        writePage deck 2 button page2

-- DeviceInfo { path = "/dev/hidraw%d"
--            , vendorId = 4057
--            , produc tId = 96
--            , serialNumber = Just ""
--            , releaseNumber = 256
--            , manufacturerString = Just "Elgato Systems"
--            , productString = Just "Stream Deck"
--            , usagePage = 13410
--            , usage = 13359
--            , interfaceNumber = 0
--            }
enumerateStreamDecks :: IO [HID.DeviceInfo]
enumerateStreamDecks = HID.enumerate (Just vendorID) (Just productID)

openStreamDeck :: HID.DeviceInfo -> IO Deck
openStreamDeck device = HID.withHIDAPI $ do
    deck <- HID.openDeviceInfo device
    sn <- determineSN device deck
    fw <- requestFW deck
    return Deck { _ref = deck
                , serialNumber = sn
                , firmware = fw
                , display = defaultPage
                }

determineSN :: HID.DeviceInfo -> HID.Device -> IO BS.ByteString
determineSN info dev = case HID.serialNumber info of
    Nothing -> requestSN dev
    Just "" -> requestSN dev
    Just sn -> return $ BSC.pack $ show sn

requestSN :: HID.Device -> IO BS.ByteString
requestSN dev = do
    sn <- HID.getFeatureReport dev 3 17
    return $ BS.takeWhile (/= 0) $ BS.drop 5 $ snd sn

requestFW :: HID.Device -> IO BS.ByteString
requestFW dev = do
    fw <- HID.getFeatureReport dev 4 17
    return $ BS.takeWhile (/= 0) $ BS.drop 5 $ snd fw

drawRow :: Deck -> DW.Word8 -> Row -> IO ()
drawRow d r (Row (i0, i1, i2, i3, i4)) = do
    writeImage d (r * 5) i0
    writeImage d (r * 5 + 1) i1
    writeImage d (r * 5 + 2) i2
    writeImage d (r * 5 + 3) i3
    writeImage d (r * 5 + 4) i4

drawPage :: Deck -> IO ()
drawPage d = do
    drawRow d 0 r0
    drawRow d 1 r1
    drawRow d 2 r2
  where
    Page (r0, r1, r2) = display d

updateDeck :: Deck -> (Page -> Page) -> IO Deck
updateDeck d f =
    let newPage = f $ display d
        newDeck = d { display = newPage }
    in do
    _ <- drawPage newDeck
    return newDeck
