module Main where

import Sound.RubberBand.Nice
import Sound.RubberBand.Option (defaultOptions)

import Data.WAVE
import System.Environment (getArgs)
import Control.Monad (forM, forM_)
import Data.List (transpose)
import Data.Array.Storable
import Foreign.C.Types (CFloat)

toArrays :: WAVESamples -> IO [StorableArray Int CFloat]
toArrays ws = let
  lists = map (map $ realToFrac . sampleToDouble) $ transpose ws
  in forM lists $ \fs -> newListArray (0, length fs - 1) fs

fromArrays :: [StorableArray Int CFloat] -> IO WAVESamples
fromArrays ars = do
  lists <- mapM getElems ars
  return $ transpose $ map (map $ doubleToSample . realToFrac) lists

main :: IO ()
main = do
  argv <- getArgs
  (fin, fout, ratio) <- case argv of
    [x, y, z] -> return (x, y, read z)
    _ -> error "invalid usage"
  WAVE (WAVEHeader nchannels fps bits nframes) samples <- getWAVEFile fin
  s <- new (fromIntegral fps) (fromIntegral nchannels) defaultOptions ratio 1
  --setDebugLevel s 3
  case nframes of
    Just f -> setExpectedInputDuration s $ fromIntegral f
    Nothing -> error "no number of frames"

  let blocks = groupsOf 1024 samples
  forM_ (init blocks) $ \block -> do
    ars <- toArrays block
    study s ars False
  forM_ [last blocks] $ \block -> do
    ars <- toArrays block
    study s ars True

  ws <- processRetrieveAll s samples
  let wav' = WAVE (WAVEHeader nchannels fps bits Nothing) ws
  putWAVEFile fout wav'

processRetrieveAll :: State -> WAVESamples -> IO WAVESamples
processRetrieveAll s samps = do
  avail <- available s
  case avail of
    Nothing -> if null samps
      then return []
      else do
        putStrLn "done processing but we have samples left"
        return []
    Just 0 -> do
      req <- getSamplesRequired s
      if req == 0
        then do
          putStrLn "thinking"
          processRetrieveAll s samps
        else case splitAt req samps of
          ([]   , _   ) -> do
            putStrLn "needs samples but we don't have any"
            processRetrieveAll s samps
          (block, rest) -> do
            ars <- toArrays block
            process s ars $ null rest
            processRetrieveAll s rest
    Just n -> do
      ars <- retrieve s n
      ws <- fromArrays ars
      rest <- processRetrieveAll s samps
      return $ ws ++ rest

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = case splitAt n xs of
  ([], []) -> []
  (ys, zs) -> ys : groupsOf n zs
