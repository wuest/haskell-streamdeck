{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Streamdeck ( Deck (..)
                                  , Page (..)
                                  , ConnectedDecks (..)
                                  , Image
                                  , Pixel
                                  , enumerate
                                  , open
                                  , serialNumber, firmware
                                  , sendRaw
                                  , writeImage
                                  , update
                                  , setBrightness
                                  , readButtonState
                                  , solidRGB
                                  ) where

import qualified Data.Bits             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.List.Split       as Split (chunksOf)
import qualified Data.Word             as DW   (Word16, Word8)
import qualified System.HIDAPI         as HID

import Prelude

type MiniActiveRow = (Bool, Bool, Bool)
type SDActiveRow   = (Bool, Bool, Bool, Bool, Bool)
type XLActiveRow   = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type MiniActiveMap = (MiniActiveRow, MiniActiveRow)
type SDActiveMap   = (SDActiveRow, SDActiveRow, SDActiveRow)
type XLActiveMap   = (XLActiveRow, XLActiveRow, XLActiveRow, XLActiveRow)

data ButtonsActive = MiniActive MiniActiveMap
                   | SDActive SDActiveMap
                   | XLActive XLActiveMap
                   deriving (Show, Eq)

type Image  = BS.ByteString
type PixelR = DW.Word8
type PixelG = DW.Word8
type PixelB = DW.Word8
type Pixel  = (PixelR, PixelG, PixelB)

type MiniRow = (Image, Image, Image)
type SDRow   = (Image, Image, Image, Image, Image)
type XLRow   = (Image, Image, Image, Image, Image, Image, Image, Image)

data PageType = MiniPage | SDPage | XLPage
data Page :: PageType -> * where
    MiniPage' :: (MiniRow, MiniRow) -> Page 'MiniPage
    SDPage'   :: (SDRow, SDRow, SDRow) -> Page 'SDPage
    XLPage'   :: (XLRow, XLRow, XLRow, XLRow) -> Page 'XLPage

data DeckSpec = DeckSpec { _ref          :: HID.Device
                         , _serialNumber :: BS.ByteString
                         , _firmware     :: BS.ByteString
                         }

data DeckType = StreamDeckMini | StreamDeck | StreamDeckXL
data Deck :: DeckType -> * where
    StreamDeckMini' :: (HID.DeviceInfo, Maybe DeckSpec, Page 'MiniPage) -> Deck 'StreamDeckMini
    StreamDeck'     :: (HID.DeviceInfo, Maybe DeckSpec, Page 'SDPage) -> Deck 'StreamDeck
    StreamDeckXL'   :: (HID.DeviceInfo, Maybe DeckSpec, Page 'XLPage) -> Deck 'StreamDeckXL

data ConnectedDecks = ConnectedDecks { minis :: [Deck 'StreamDeckMini]
                                     , decks :: [Deck 'StreamDeck]
                                     , xls   :: [Deck 'StreamDeckXL]
                                     }

-- Example Device Info for an early Stream Deck:
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

-- Constant across all Elgato hardware
vendorID :: Maybe DW.Word16
vendorID = Just 0x0fd9

-- Enumerate Stream Decks, differentiating by type
enumerate :: IO ConnectedDecks
enumerate = do
    m <- enum (Just 0x0063)
    s <- enum (Just 0x0060)
    x <- enum (Just 0x006c)
    return $ ConnectedDecks { minis = fmap fm m
                            , decks = fmap fs s
                            , xls   = fmap fx x
                            }
        where enum = HID.enumerate vendorID
              fm di = StreamDeckMini' (di, Nothing, emptyMiniPage)
              fs di = StreamDeck' (di, Nothing, emptySDPage)
              fx di = StreamDeckXL' (di, Nothing, emptyXLPage)

-- Note: older firmware Stream Decks have a bug where "" is reported as the
-- Serial number unless a feature report requesting the Serial Number is issued.
-- Newer Stream Decks do not require this step.
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

open :: Deck a -> IO (Deck a)
open (StreamDeckMini' (device, Nothing, _)) =
    open' device (\deck sn fw -> StreamDeckMini' (device, Just $ DeckSpec { _ref = deck, _serialNumber = sn, _firmware = fw }, emptyMiniPage) )
open (StreamDeck' (device, Nothing, _)) =
    open' device (\deck sn fw -> StreamDeck' (device, Just $ DeckSpec { _ref = deck, _serialNumber = sn, _firmware = fw }, emptySDPage) )
open (StreamDeckXL' (device, Nothing, _)) =
    open' device (\deck sn fw -> StreamDeckXL' (device, Just $ DeckSpec { _ref = deck, _serialNumber = sn, _firmware = fw }, emptyXLPage) )
open _ = undefined -- Opening an already-opened Stream Deck is undefined

open' :: HID.DeviceInfo -> (HID.Device -> BS.ByteString -> BS.ByteString -> Deck a) -> IO (Deck a)
open' device f = HID.withHIDAPI $ do
    deck <- HID.openDeviceInfo device
    sn <- determineSN device deck
    fw <- requestFW deck
    return $ f deck sn fw

serialNumber :: Deck a -> BS.ByteString
serialNumber (StreamDeckMini' (_, Just ds, _)) = _serialNumber ds
serialNumber (StreamDeck' (_, Just ds, _)) = _serialNumber ds
serialNumber (StreamDeckXL' (_, Just ds, _)) = _serialNumber ds
serialNumber _ = undefined

firmware :: Deck a -> BS.ByteString
firmware (StreamDeckMini' (_, Just ds, _)) = _firmware ds
firmware (StreamDeck' (_, Just ds, _)) = _firmware ds
firmware (StreamDeckXL' (_, Just ds, _)) = _firmware ds
firmware _ = undefined

-- Constants useful in communicating with Stream Decks
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

emptyMiniRow :: MiniRow
emptyMiniRow = (b, b, b)
    where b = solidRGB 0 0 0
emptySDRow :: SDRow
emptySDRow =  (b, b, b, b, b)
    where b = solidRGB 0 0 0
emptyXLRow :: XLRow
emptyXLRow = (b, b, b, b, b, b, b, b)
    where b = solidRGB 0 0 0

emptyMiniPage :: Page 'MiniPage
emptyMiniPage = MiniPage' (r, r)
    where r = emptyMiniRow
emptySDPage :: Page 'SDPage
emptySDPage = SDPage' (r, r, r)
    where r = emptySDRow
emptyXLPage :: Page 'XLPage
emptyXLPage = XLPage' (r, r, r, r)
    where r = emptyXLRow

-- Helper method to return the device for a given Deck
ref :: Deck a -> HID.Device
ref (StreamDeckMini' (_, Just spec, _)) = _ref spec
ref (StreamDeck' (_, Just spec, _)) = _ref spec
ref (StreamDeckXL' (_, Just spec, _)) = _ref spec
ref _ = undefined -- requesting a reference to an unopened Deck is undefined

-- Send raw bytes to a Deck, broken up by packets
sendRaw :: Deck a -> BS.ByteString -> IO ()
sendRaw deck bs =
    if BS.length bs > packetSize then
        (do _ <- HID.write (ref deck) $ BS.take packetSize bs
            sendRaw deck $ fixContinuationPacket bs)
    else
        (do _ <- HID.write (ref deck) bs
            return ())

-- In cases where the first byte of a continuation packet is equal to 0, the
-- byte is discarded entirely, resulting in discoloration.  Continuation packets
-- only get used in cases where data > 4096 bytes is being sent, so in practice
-- this is only a concern for sending image data.
fixContinuationPacket :: BS.ByteString -> BS.ByteString
fixContinuationPacket b
    | BS.length b < packetSize = BS.pack []
    | otherwise =
        let rest = BS.drop packetSize b
            byte = BS.head rest
        in if byte > 0 then rest
                       else BS.cons ((B..|.) 1 byte) (BS.drop 1 rest)

-- Generate a packet to set the Deck's brightness.  Values passed are bracketed
-- to 0 >= x >= 100.  Full brightness is defaulted to otherwise.
setBrightness :: DW.Word8 -> BS.ByteString
setBrightness b
    | b <= 100 && b >= 0 = setBrightness' b
    | otherwise = setBrightness' 100

setBrightness' :: DW.Word8 -> BS.ByteString
setBrightness' b = BS.pack [ 0x05                   -- Report 0x05
                           , 0x55, 0xAA, 0xD1, 0x01 -- Command (brightness)
                           ,    b, 0x00, 0x00, 0x00 -- brightness
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           ]

-- Write a given page of data to a Deck.
writePage :: Deck a -> Int -> DW.Word8 -> BS.ByteString -> IO ()
writePage deck p i bs = sendRaw deck $ BS.append (page p i) bs

-- Generate a given page of data to be sent to a Deck.  The first page of a
-- command (for commands requiring 2 pages - typically sending image data) has
-- a longer prologue than the second page.  No commands with more than 2 pages
-- exist.
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

-- Helper: read data from a given Deck
readRaw :: HID.Device -> Int -> IO BS.ByteString
readRaw = HID.read

-- Stream Deck reports button state ONLY upon button press/release
-- Stream Deck will send a 16 byte message, with the following format:
-- 01 AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO
-- * Byte 0 being set to 0x01 is static, indicating a "button event" message.
-- * AA-OO are 1 byte, if the low bit is set, the button is pressed.  Bits 1-7
--   appear to be unused.
-- Note: The Stream Deck Mini will not send data beyond the 8th byte
-- Note: The Stream Deck XL's format is slightly different.  The Stream Deck XL
--       sends a prelude of 01 00 20 00, followed by 32 bytes, following the
--       above pattern - first byte for the first (top-left) button, next for
--       the button to its right, and so on for a total of 36 bytes.
readButtonState :: Deck a -> IO ButtonsActive
readButtonState (StreamDeck' (_, Just ds, _)) =
    bytesToActiveMap . BS.unpack <$> readRaw (_ref ds) 16
readButtonState (StreamDeckMini' (_, Just ds, _)) =
    bytesToActiveMap . BS.unpack <$> readRaw (_ref ds) 7
readButtonState (StreamDeckXL' (_, Just ds, _)) =
    bytesToActiveMap . BS.unpack <$> readRaw (_ref ds) 36
readButtonState _ = undefined

bytesToActiveMap :: [DW.Word8] -> ButtonsActive
bytesToActiveMap xs
    | length xs == 16 = sdMap $   map (== 1) $ drop 1 xs
    | length xs == 7  = miniMap $ map (== 1) $ drop 1 xs
    | length xs == 36 = xlMap $   map (== 1) $ drop 4 xs
    | otherwise = undefined

sdMap :: [Bool] -> ButtonsActive
sdMap bs = SDActive $ sdam $ sdar <$> Split.chunksOf 5 bs
    where
        sdar :: [Bool] -> SDActiveRow
        sdar [a, b, c, d, e] = (a,b,c,d,e)
        sdar _ = undefined
        sdam :: [SDActiveRow] -> SDActiveMap
        sdam [a, b, c] = (a,b,c)
        sdam _ = undefined

miniMap :: [Bool] -> ButtonsActive
miniMap bs = MiniActive $ miniam $ miniar <$> Split.chunksOf 3 bs
    where
        miniar :: [Bool] -> MiniActiveRow
        miniar [a, b, c] = (a,b,c)
        miniar _ = undefined
        miniam :: [MiniActiveRow] -> MiniActiveMap
        miniam [a, b] = (a,b)
        miniam _ = undefined

xlMap :: [Bool] -> ButtonsActive
xlMap bs = XLActive $ xlam $ xlar <$> Split.chunksOf 8 bs
    where
        xlar :: [Bool] -> XLActiveRow
        xlar [a, b, c, d, e, f, g, h] = (a,b,c,d,e,f,g,h)
        xlar _ = undefined
        xlam :: [XLActiveRow] -> XLActiveMap
        xlam [a, b, c, d] = (a,b,c,d)
        xlam _ = undefined

writeImage :: Deck a -> DW.Word8 -> Image -> IO ()
writeImage deck button img =
    let page1 = BS.take (3 * page1Pixels) img
        page2 = BS.take (3 * page2Pixels) $ BS.drop (3 * page1Pixels) img
    in writePage deck 1 button page1 >> writePage deck 2 button page2

drawMiniRow :: Deck 'StreamDeckMini -> DW.Word8 -> MiniRow -> IO ()
drawMiniRow d r (i0, i1, i2) = do
    writeImage d (r * 3) i0
    writeImage d (r * 3 + 1) i1
    writeImage d (r * 3 + 2) i2

drawSDRow :: Deck 'StreamDeck -> DW.Word8 -> SDRow -> IO ()
drawSDRow d r (i0, i1, i2, i3, i4) = do
    writeImage d (r * 5) i0
    writeImage d (r * 5 + 1) i1
    writeImage d (r * 5 + 2) i2
    writeImage d (r * 5 + 3) i3
    writeImage d (r * 5 + 4) i4

drawXLRow :: Deck 'StreamDeckXL -> DW.Word8 -> XLRow -> IO ()
drawXLRow d r (i0, i1, i2, i3, i4, i5, i6, i7) = do
    writeImage d (r * 8) i0
    writeImage d (r * 8 + 1) i1
    writeImage d (r * 8 + 2) i2
    writeImage d (r * 8 + 3) i3
    writeImage d (r * 8 + 4) i4
    writeImage d (r * 8 + 5) i5
    writeImage d (r * 8 + 6) i6
    writeImage d (r * 8 + 7) i7

render :: Deck a -> IO ()
render (StreamDeckMini' (dev, Just ds, display)) = drawRow d 0 r0 >> drawRow d 1 r1
  where MiniPage' (r0, r1) = display
        drawRow = drawMiniRow
        d = StreamDeckMini' (dev, Just ds, display)
render (StreamDeck' (dev, Just ds, display)) = drawRow d 0 r0 >> drawRow d 1 r1 >> drawRow d 2 r2
  where SDPage' (r0, r1, r2) = display
        drawRow = drawSDRow
        d = StreamDeck' (dev, Just ds, display)
render (StreamDeckXL' (dev, Just ds, display)) = drawRow d 0 r0 >> drawRow d 1 r1 >> drawRow d 2 r2 >> drawRow d 3 r3
  where XLPage' (r0, r1, r2, r3) = display
        drawRow = drawXLRow
        d = StreamDeckXL' (dev, Just ds, display)
render _ = undefined

update :: Deck a -> Page b -> IO (Deck a)
update (StreamDeckMini' (di, Just ds, _)) (MiniPage' p) = do
    let new = StreamDeckMini' (di, Just ds, MiniPage' p)
    render new >> return new
update (StreamDeck' (di, Just ds, _)) (SDPage' p) = do
    let new = StreamDeck' (di, Just ds, SDPage' p)
    render new >> return new
update (StreamDeckXL' (di, Just ds, _)) (XLPage' p) = do
    let new = StreamDeckXL' (di, Just ds, XLPage' p)
    render new >> return new
update _ _ = undefined
