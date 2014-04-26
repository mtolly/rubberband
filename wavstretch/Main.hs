module Main (main) where

import Sound.RubberBand.Nice
import Sound.RubberBand.Option (defaultOptions)

import System.Environment (getArgs)
import Control.Monad (replicateM_, when)
import Data.List (transpose)

import Sound.File.Sndfile
import Sound.File.Sndfile.Buffer.Vector
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  argv <- getArgs
  (fin, fout, trat, frat) <- case argv of
    [a, b, c] -> return (a, b, read c, 1)
    [a, b, c, d] -> return (a, b, read c, read d)
    _ -> error "Usage: wavstretch in.wav out.wav time-ratio [freq-ratio]"
  hndl <- openFile fin ReadMode defaultInfo
  let Info nframes fps nchannels _fmt nsections skbl = hInfo hndl
  when (not skbl) $ error "File is somehow not seekable?"
  s <- new (fromIntegral fps) (fromIntegral nchannels) defaultOptions trat frat
  --setDebugLevel s 3
  setExpectedInputDuration s nframes

  let blockSize = 1024
      (nonFinalBlocks, finalBlock) = case quotRem nframes blockSize of
        (q, 0) -> (q - 1, blockSize)
        (q, r) -> (q    , r        )

  putStrLn $ "Supplying " ++ show (nonFinalBlocks + 1) ++ " blocks..."
  replicateM_ nonFinalBlocks $ do
    block <- getDeinterleaved hndl blockSize
    study s block False
  block <- getDeinterleaved hndl finalBlock
  study s block True
  putStrLn "Done!"

  seekPos <- hSeek hndl AbsoluteSeek 0
  when (seekPos /= 0) $ error "File somehow was not seeked to 0"

  hndl2 <- openFile fout WriteMode $ Info
    { frames = floor $ fromIntegral nframes * trat
    , samplerate = fps
    , channels = nchannels
    , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
    , sections = nsections
    , seekable = True
    }
  processRetrieveAll 0 nframes s hndl hndl2

getDeinterleaved :: Handle -> Count -> IO [V.Vector Float]
getDeinterleaved h c = do
  let nchannels = channels $ hInfo h
  buf <- hGetBuffer h c
  case buf of
    Nothing -> return $ replicate nchannels V.empty
    Just b  -> return $ let
      frameLists = groupsOf nchannels $ V.toList $ fromBuffer b
      in [ V.fromList $ map (!! i) frameLists | i <- [0 .. nchannels - 1] ]

interleave :: [V.Vector Float] -> V.Vector Float
interleave = V.concat . map V.fromList . transpose . map V.toList

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = case splitAt n xs of
  ([], []) -> []
  (ys, zs) -> ys : groupsOf n zs

processRetrieveAll :: Count -> Count -> Stretcher -> Handle -> Handle -> IO ()
processRetrieveAll pos len s hin hout = do
  avail <- available s
  case avail of
    Nothing -> return ()
    Just 0 -> do
      req <- getSamplesRequired s
      if req == 0
        then do
          --putStrLn "Thinking"
          processRetrieveAll pos len s hin hout
        else do
          let isFinal = len - pos <= req
          if pos == len
            then do
              --putStrLn "We don't have any samples to provide"
              processRetrieveAll pos len s hin hout
            else do
              chans <- getDeinterleaved hin req
              process s chans isFinal
              let got = V.length $ head chans
              processRetrieveAll (pos + got) len s hin hout
    Just n -> do
      bufs <- retrieve s n
      _ <- hPutBuffer hout $ toBuffer $ interleave bufs
      processRetrieveAll pos len s hin hout
