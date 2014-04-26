{- |

Threading notes for real-time applications:

Multiple instances of 'Stretcher' may be created and used
in separate threads concurrently.  However, for any single instance
of 'Stretcher', you may not call 'process' more than once
concurrently, and you may not change the time or pitch ratio while
a 'process' call is being executed (if the stretcher was created in
"real-time mode"; in "offline mode" you can't change the ratios
during use anyway).

So you can run 'process' in its own thread if you like, but if you
want to change ratios dynamically from a different thread, you will
need some form of mutex in your code.  Changing the time or pitch
ratio is real-time safe except in extreme circumstances, so for
most applications that may change these dynamically it probably
makes most sense to do so from the same thread as calls 'process',
even if that is a real-time thread.

Differences from "Sound.RubberBand.Raw":

  * The 'Stretcher' object is garbage-collected by Haskell.

  * The 'study', 'process', and 'retrieve' functions use storable
    'Vector's instead of raw pointers.

  * Some error checking is done in cases like giving arrays of different
    lengths to 'study' and 'process', or giving a different number of arrays
    from how many channels the 'Stretcher' was constructed with.

-}
module Sound.RubberBand.Nice

( Stretcher()
, withRaw

, new, reset

, setTimeRatio, setPitchScale
, getTimeRatio, getPitchScale
, getLatency

, setTransientsOption
, setDetectorOption
, setPhaseOption
, setFormantOption
, setPitchOption

, setExpectedInputDuration
, getSamplesRequired

, setMaxProcessSize
, setKeyFrameMap

, study, process
, available, retrieve

, getChannelCount

, calculateStretch

, setDebugLevel
, setDefaultDebugLevel

) where

import qualified Sound.RubberBand.Raw as Raw
import Sound.RubberBand.Raw (SampleRate, NumChannels, TimeRatio, PitchScale)
import Sound.RubberBand.Option

import Foreign
  (Ptr, ForeignPtr, newForeignPtr, withForeignPtr, finalizerFree, castPtr)
import Control.Applicative ((<$>))
import Foreign.Marshal.Array (withArray, withArrayLen, mallocArray)
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CFloat)
import Control.Monad (guard, forM, replicateM)

newtype Stretcher = Stretcher (ForeignPtr Raw.Stretcher)
  deriving (Eq, Ord, Show)

withRaw :: Stretcher -> (Raw.Stretcher -> IO a) -> IO a
withRaw (Stretcher fp) f = withForeignPtr fp $ f . Raw.Stretcher

new :: SampleRate -> NumChannels -> Options -> TimeRatio -> PitchScale ->
  IO Stretcher
new a b c d e = do
  Raw.Stretcher p <- Raw.new a b c d e
  Stretcher <$> newForeignPtr Raw.p_delete p

reset :: Stretcher -> IO ()
reset s = withRaw s Raw.reset

setTimeRatio :: Stretcher -> TimeRatio -> IO ()
setTimeRatio s d = withRaw s $ \r -> Raw.setTimeRatio r d

setPitchScale :: Stretcher -> PitchScale -> IO ()
setPitchScale s d = withRaw s $ \r -> Raw.setPitchScale r d

getTimeRatio :: Stretcher -> IO TimeRatio
getTimeRatio s = withRaw s Raw.getTimeRatio

getPitchScale :: Stretcher -> IO PitchScale
getPitchScale s = withRaw s Raw.getPitchScale

getLatency :: Stretcher -> IO Int
getLatency s = withRaw s Raw.getLatency

setTransientsOption :: Stretcher -> Transients -> IO ()
setTransientsOption s o = withRaw s $ \r -> Raw.setTransientsOption r o

setDetectorOption :: Stretcher -> Detector -> IO ()
setDetectorOption s o = withRaw s $ \r -> Raw.setDetectorOption r o

setPhaseOption :: Stretcher -> Phase -> IO ()
setPhaseOption s o = withRaw s $ \r -> Raw.setPhaseOption r o

setFormantOption :: Stretcher -> Formant -> IO ()
setFormantOption s o = withRaw s $ \r -> Raw.setFormantOption r o

setPitchOption :: Stretcher -> Pitch -> IO ()
setPitchOption s o = withRaw s $ \r -> Raw.setPitchOption r o

setExpectedInputDuration :: Stretcher -> Int -> IO ()
setExpectedInputDuration s n =
  withRaw s $ \r -> Raw.setExpectedInputDuration r n

getSamplesRequired :: Stretcher -> IO Int
getSamplesRequired s = withRaw s Raw.getSamplesRequired

setMaxProcessSize :: Stretcher -> Int -> IO ()
setMaxProcessSize s n = withRaw s $ \r -> Raw.setMaxProcessSize r n

setKeyFrameMap :: Stretcher -> [(Int, Int)] -> IO ()
setKeyFrameMap s pairs = withRaw s $ \r ->
  withArray (map (fromIntegral . fst) pairs) $ \p1 ->
    withArray (map (fromIntegral . snd) pairs) $ \p2 ->
      Raw.setKeyFrameMap r (length pairs) p1 p2

unsafeWiths :: (V.Storable e) => [V.Vector e] -> ([Ptr e] -> IO a) -> IO a
unsafeWiths []       f = f []
unsafeWiths (x : xs) f =
  V.unsafeWith x $ \p ->
    unsafeWiths xs $ \ps ->
      f $ p : ps

getUniform :: (Eq a) => [a] -> Maybe a
getUniform (x : xs) = guard (all (== x) xs) >> Just x
getUniform []       = Nothing

-- | Ugly, but needed to share the code for 'study' and 'process'.
studyProcess ::
  String -> (Raw.Stretcher -> Ptr (Ptr CFloat) -> Int -> Bool -> IO ()) ->
    Stretcher -> [V.Vector Float] -> Bool -> IO ()
studyProcess fname f s chans final = do
  samples <- case getUniform $ map V.length chans of
    Nothing -> error $ fname ++ ": " ++ if null chans
      then "no input arrays given"
      else "input arrays have differing lengths"
    Just sam -> return sam
  unsafeWiths chans $ \pfs ->
    withArrayLen pfs $ \len ppf -> do
      numchans <- getChannelCount s
      if numchans == len
        then withRaw s $ \r -> f r (castPtr ppf) samples final
        else error $ unwords
          [ fname ++ ": passed"
          , show len
          , "channels but Stretcher needs"
          , show numchans
          ]

study :: Stretcher -> [V.Vector Float] -> Bool -> IO ()
study = studyProcess "study" Raw.study

process :: Stretcher -> [V.Vector Float] -> Bool -> IO ()
process = studyProcess "process" Raw.process

-- | Returns 'Nothing' if all data has been fully processed.
available :: Stretcher -> IO (Maybe Int)
available s = withRaw s $ \r -> do
  i <- Raw.available r
  return $ guard (i /= (-1)) >> Just i

retrieveInto :: Stretcher -> [Ptr Float] -> Int -> IO Int
retrieveInto s pfs samples = do
  numchans <- getChannelCount s
  withArrayLen pfs $ \len ppf ->
    if len == numchans
      then withRaw s $ \r -> Raw.retrieve r (castPtr ppf) samples
      else error $ unwords
        [ "retrieveInto: passed"
        , show len
        , "channels but Stretcher needs"
        , show numchans
        ]

retrieve :: Stretcher -> Int -> IO [V.Vector Float]
retrieve s samples = do
  numchans <- getChannelCount s
  ps <- replicateM numchans $ mallocArray samples
  actual <- retrieveInto s ps samples
  forM ps $ \p -> do
    fp <- newForeignPtr finalizerFree $ castPtr p
    return $ V.unsafeFromForeignPtr0 fp actual

getChannelCount :: Stretcher -> IO Int
getChannelCount s = withRaw s Raw.getChannelCount

calculateStretch :: Stretcher -> IO ()
calculateStretch s = withRaw s Raw.calculateStretch

setDebugLevel :: Stretcher -> Int -> IO ()
setDebugLevel s n = withRaw s $ \r -> Raw.setDebugLevel r n

setDefaultDebugLevel :: Int -> IO ()
setDefaultDebugLevel = Raw.setDefaultDebugLevel
