module Main where

import System.IO
import System.Environment
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.List as L

import Diagrams.Prelude
import Diagrams.Backend.SVG hiding (B)
import Diagrams.Backend.Cairo hiding (B)
import Diagrams.Backend.SVG.CmdLine
--import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Arc
import Diagrams.Angle

import qualified Data.Colour as C
import Data.Colour.RGBSpace.HSV

import Data.Word
import GHC.Float

colorMap :: [Colour Double]
colorMap =
  black:[sRGB24 r g b | i <- [0..255], let hsvColor = hsv (fromIntegral $ i+45 `mod` 360) 1 1 :: RGB Double, let RGB r g b = fmap (round . (255*)) hsvColor :: RGB Word8]

-- | Draws the color map to be included as a the legend
drawColorMap :: Diagram B
drawColorMap =
  let
    maxColor = length colorMap -1
    colorBar = hcat [rect 4 2 # fc (colorMap !! colorIndex) # lw none | colorIndex <- [0..maxColor]] :: Diagram B
    labelText = hsep 49 [text (show index) # fc (colorMap !! index) # fontSizeL 10 <> if index == 0 then rect 15 15 # lw none # bg white else rect 15 40 # lw none | index <- [0..length colorMap -1], index `mod` 16 == 0] :: Diagram B
  in
    colorBar === labelText
    
-- | Reads a single track, i.e. number of sectors * 256. The number of sectors depends on the track index
readTrack :: Int -> Handle -> IO BS.ByteString
readTrack track handle
  | 1 <= track && track <= 17 = BS.hGet handle $ 21 * 256
  | 18 <= track && track <= 24 = BS.hGet handle $ 19 * 256
  | 25 <= track && track <= 30 = BS.hGet handle $ 18 * 256
  | 31 <= track && track <= 40 = BS.hGet handle $ 17 * 256

readDisk :: Handle -> IO [BS.ByteString]
readDisk handle = sequence [readTrack track handle | track <- [1..40]]

showDisk :: [BS.ByteString] -> IO ()
showDisk bss =
  putStrLn $ unlines $ L.map show [BS.unpack bs | bs <- bss]


-- | Draws a single track
drawTrack :: Double -> Double -> Word8 -> BS.ByteString -> [Diagram B]
drawTrack diskRadius trackWidth trackId trackData =
  let
    radiusOuter = diskRadius - ((word2Double $ fromIntegral (trackId - 1)) * trackWidth+5)
    radiusInner = radiusOuter - trackWidth
  in
    [annularWedge radiusOuter radiusInner xDir ((sectorId/len :: Double) @@ turn) # lw none # fc (colorMap !! fromIntegral sectorData) | (sectorId, sectorData) <- zip [1..] (BS.unpack trackData), let len=fromIntegral $ BS.length trackData]

-- | Draws a complete disk given its data as a list of tracks
drawDisk :: [BS.ByteString] -> Diagram B
drawDisk tracks =
  let
    radius = 800
    trackWidth = 20
    -- The list of tracks together with track numbers
    diskData = zip [1..] tracks :: [(Word8, BS.ByteString)]
  in
    drawColorMap === ((circle 810 # lw none # fc grey) `beneath` (foldl atop  (circle 5 # lw none # fc white <> circle 95 # lw none # fc grey) $ concat [drawTrack radius trackWidth trackId trackData | (trackId,trackData) <- diskData]))

-- main' =
--   renderSVG "ddt.svg" absolute =<<
--     withFile "ddt.d64" ReadMode
--       (return . drawDisk <=< readDisk)
-- main'' =
--   renderCairo "ddt.png" absolute =<<
--     withFile "ddt.d64" ReadMode
--       (return . drawDisk <=< readDisk)

main =
  let
--    fileName = "ddt.d64"
    fileName = "Star_Wars.d64"
  in
    mainWith =<<
      withFile fileName ReadMode
      (return . drawDisk <=< readDisk)
