module Main where

import Sound.RubberBand.Nice
import Sound.RubberBand.Option (defaultOptions)

import Data.WAVE
import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.List (transpose)

import qualified Data.Vector.Storable as V

toArrays :: WAVESamples -> [V.Vector Float]
toArrays ws = do
  chan <- transpose ws
  return $ V.fromList $ map (realToFrac . sampleToDouble) chan

fromArrays :: [V.Vector Float] -> WAVESamples
fromArrays ars = transpose $ do
  ar <- ars
  return $ map (doubleToSample . realToFrac) $ V.toList ar

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

  putStrLn "Supplying blocks..."
  forM_ (zip [0..] blocks) $ \(i, block) -> do
    study s (toArrays block) False
    print (i :: Int)
  study s (replicate nchannels V.empty) True
  putStrLn "Done!"

  WAVE _ samples' <- getWAVEFile fin

  ws <- processRetrieveAll s samples'
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
            process s (toArrays block) $ null rest
            processRetrieveAll s rest
    Just n -> do
      ars <- retrieve s n
      rest <- processRetrieveAll s samps
      return $ fromArrays ars ++ rest

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = case splitAt n xs of
  ([], []) -> []
  (ys, zs) -> ys : groupsOf n zs
