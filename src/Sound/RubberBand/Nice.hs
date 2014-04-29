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

, new, reset
, SampleRate, NumChannels, TimeRatio, PitchScale
, withRaw

, setTimeRatio, setPitchScale
, getTimeRatio, getPitchScale
, getLatency

, setTransientsOption
, setDetectorOption
, setPhaseOption
, setFormantOption
, setPitchOption

, setExpectedInputDuration
, setMaxProcessSize
, getSamplesRequired
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

-- | An audio stretching machine. This object is garbage-collected on the
-- Haskell side, so it will be deleted automatically.
newtype Stretcher = Stretcher (ForeignPtr Raw.Stretcher)
  deriving (Eq, Ord, Show)

-- | Allows you to use the functions in "Sound.RubberBand.Raw" if needed.
withRaw :: Stretcher -> (Raw.Stretcher -> IO a) -> IO a
withRaw (Stretcher fp) f = withForeignPtr fp $ f . Raw.Stretcher

{- |
Construct a time and pitch stretcher object to run at the given
sample rate, with the given number of channels.  Processing
options and the time and pitch scaling ratios may be provided.
The time and pitch ratios may be changed after construction,
but most of the options may not.  See the "Sound.RubberBand.Option"
documentation for more details.
-}
new :: SampleRate -> NumChannels -> Options -> TimeRatio -> PitchScale ->
  IO Stretcher
new a b c d e = do
  Raw.Stretcher p <- Raw.new a b c d e
  Stretcher <$> newForeignPtr Raw.p_delete p

{- |
Reset the stretcher's internal buffers.  The stretcher should
subsequently behave as if it had just been constructed
(although retaining the current time and pitch ratio).
-}
reset :: Stretcher -> IO ()
reset s = withRaw s Raw.reset

{- |
Set the time ratio for the stretcher.  This is the ratio of
stretched to unstretched duration -- not tempo.  For example, a
ratio of 2.0 would make the audio twice as long (i.e. halve the
tempo); 0.5 would make it half as long (i.e. double the tempo);
1.0 would leave the duration unaffected.

If the stretcher was constructed in 'Offline' mode, the time
ratio is fixed throughout operation; this function may be
called any number of times between construction (or a call to
'reset') and the first call to 'study' or 'process', but may
not be called after 'study' or 'process' has been called.

If the stretcher was constructed in 'RealTime' mode, the time
ratio may be varied during operation; this function may be
called at any time, so long as it is not called concurrently
with 'process'.  You should either call this function from the
same thread as 'process', or provide your own mutex or similar
mechanism to ensure that 'setTimeRatio' and 'process' cannot be
run at once (there is no internal mutex for this purpose).
-}
setTimeRatio :: Stretcher -> TimeRatio -> IO ()
setTimeRatio s d = withRaw s $ \r -> Raw.setTimeRatio r d

{- |
Set the pitch scaling ratio for the stretcher.  This is the
ratio of target frequency to source frequency.  For example, a
ratio of 2.0 would shift up by one octave; 0.5 down by one
octave; or 1.0 leave the pitch unaffected.

To put this in musical terms, a pitch scaling ratio
corresponding to a shift of @s@ equal-tempered semitones (where @s@
is positive for an upwards shift and negative for downwards) is
@2 ** (s / 12)@.

If the stretcher was constructed in 'Offline' mode, the pitch
scaling ratio is fixed throughout operation; this function may
be called any number of times between construction (or a call
to 'reset') and the first call to 'study' or 'process', but may
not be called after 'study' or 'process' has been called.

If the stretcher was constructed in 'RealTime' mode, the pitch
scaling ratio may be varied during operation; this function may
be called at any time, so long as it is not called concurrently
with 'process'.  You should either call this function from the
same thread as 'process', or provide your own mutex or similar
mechanism to ensure that 'setPitchScale' and 'process' cannot be
run at once (there is no internal mutex for this purpose).
-}
setPitchScale :: Stretcher -> PitchScale -> IO ()
setPitchScale s d = withRaw s $ \r -> Raw.setPitchScale r d

{- |
Return the last time ratio value that was set (either on
construction or with 'setTimeRatio').
-}
getTimeRatio :: Stretcher -> IO TimeRatio
getTimeRatio s = withRaw s Raw.getTimeRatio

{- |
Return the last pitch scaling ratio value that was set (either
on construction or with 'setPitchScale').
-}
getPitchScale :: Stretcher -> IO PitchScale
getPitchScale s = withRaw s Raw.getPitchScale

{- |
Return the processing latency of the stretcher.  This is the
number of audio samples that one would have to discard at the
start of the output in order to ensure that the resulting audio
aligned with the input audio at the start.  In 'Offline' mode,
latency is automatically adjusted for and the result is zero.
In 'RealTime' mode, the latency may depend on the time and pitch
ratio and other options.
-}
getLatency :: Stretcher -> IO Int
getLatency s = withRaw s Raw.getLatency

{- |
Change a 'Transients' configuration setting.  This may be
called at any time in 'RealTime' mode.  It may not be called in
'Offline' mode (for which the 'Transients' option is fixed on
construction).
-}
setTransientsOption :: Stretcher -> Transients -> IO ()
setTransientsOption s o = withRaw s $ \r -> Raw.setTransientsOption r o

{- |
Change a 'Detector' configuration setting.  This may be
called at any time in 'RealTime' mode.  It may not be called in
'Offline' mode (for which the 'Detector' option is fixed on
construction).
-}
setDetectorOption :: Stretcher -> Detector -> IO ()
setDetectorOption s o = withRaw s $ \r -> Raw.setDetectorOption r o

{- |
Change a 'Phase' configuration setting.  This may be
called at any time in any mode.

Note that if running multi-threaded in 'Offline' mode, the change
may not take effect immediately if processing is already under
way when this function is called.
-}
setPhaseOption :: Stretcher -> Phase -> IO ()
setPhaseOption s o = withRaw s $ \r -> Raw.setPhaseOption r o

{- |
Change a 'Formant' configuration setting.  This may be
called at any time in any mode.

Note that if running multi-threaded in 'Offline' mode, the change
may not take effect immediately if processing is already under
way when this function is called.
-}
setFormantOption :: Stretcher -> Formant -> IO ()
setFormantOption s o = withRaw s $ \r -> Raw.setFormantOption r o

{- |
Change a 'Pitch' configuration setting.  This may be
called at any time in 'RealTime' mode.  It may not be called in
'Offline' mode (for which the 'Pitch' option is fixed on
construction).
-}
setPitchOption :: Stretcher -> Pitch -> IO ()
setPitchOption s o = withRaw s $ \r -> Raw.setPitchOption r o

{- |
Tell the 'Stretcher' exactly how many input samples it will
receive.  This is only useful in 'Offline' mode, when it allows
the 'Stretcher' to ensure that the number of output samples is
exactly correct.  In 'RealTime' mode no such guarantee is
possible and this value is ignored.
-}
setExpectedInputDuration :: Stretcher -> Int -> IO ()
setExpectedInputDuration s n =
  withRaw s $ \r -> Raw.setExpectedInputDuration r n

{- |
Tell the 'Stretcher' the maximum number of sample frames that you
will ever be passing in to a single 'process' call.  If you
don't call this, the 'Stretcher' will assume that you are calling
'getSamplesRequired' at each cycle and are never passing more
samples than are suggested by that function.

If your application has some external constraint that means you
prefer a fixed block size, then your normal mode of operation
would be to provide that block size to this function; to loop
calling 'process' with that size of block; after each call to
'process', test whether output has been generated by calling
'available'; and, if so, call 'retrieve' to obtain it.  See
'getSamplesRequired' for a more suitable operating mode for
applications without such external constraints.

This function may not be called after the first call to 'study'
or 'process'.

Note that this value is only relevant to 'process', not to
'study' (to which you may pass any number of samples at a time,
and from which there is no output).
-}
setMaxProcessSize :: Stretcher -> Int -> IO ()
setMaxProcessSize s n = withRaw s $ \r -> Raw.setMaxProcessSize r n

{- |
Ask the stretcher how many audio sample frames should be
provided as input in order to ensure that some more output
becomes available.

If your application has no particular constraint on processing
block size and you are able to provide any block size as input
for each cycle, then your normal mode of operation would be to
loop querying this function; providing that number of samples
to 'process'; and reading the output using 'available' and
'retrieve'.  See 'setMaxProcessSize' for a more suitable
operating mode for applications that do have external block
size constraints.

Note that this value is only relevant to 'process', not to
'study' (to which you may pass any number of samples at a time,
and from which there is no output).
-}
getSamplesRequired :: Stretcher -> IO Int
getSamplesRequired s = withRaw s Raw.getSamplesRequired

{- |
Provide a set of mappings from "before" to "after" sample
numbers so as to enforce a particular stretch profile.  The
argument is a map from audio sample frame number in the source
material, to the corresponding sample frame number in the
stretched output.  The mapping should be for key frames only,
with a "reasonable" gap between mapped samples.

This function cannot be used in 'RealTime' mode.

This function may not be called after the first call to
'process'.  It should be called after the time and pitch ratios
have been set; the results of changing the time and pitch
ratios after calling this function are undefined.  Calling
'reset' will clear this mapping.

The key frame map only affects points within the material; it
does not determine the overall stretch ratio (that is, the
ratio between the output material's duration and the source
material's duration).  You need to provide this ratio
separately to 'setTimeRatio', otherwise the results may be
truncated or extended in unexpected ways regardless of the
extent of the frame numbers found in the key frame map.
-}
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

{- |
Provide a block of "samples" sample frames for the stretcher to
study and calculate a stretch profile from.

This is only meaningful in 'Offline' mode, and is required if
running in that mode.  You should pass the entire input through
'study' before any 'process' calls are made, as a sequence of
blocks in individual 'study' calls, or as a single large block.

The input list should be de-interleaved audio data with one float vector
per channel. The 'Bool' should be 'True' if this is the last block of data
that will be provided to 'study' before the first 'process' call.
-}
study :: Stretcher -> [V.Vector Float] -> Bool -> IO ()
study = studyProcess "study" Raw.study

{- |
Provide a block of sample frames for processing.
See also 'getSamplesRequired' and 'setMaxProcessSize'.

Set the 'Bool' to 'True' if this is the last block of input data.
-}
process :: Stretcher -> [V.Vector Float] -> Bool -> IO ()
process = studyProcess "process" Raw.process

{- |
Ask the stretcher how many audio sample frames of output data
are available for reading (via 'retrieve').

This function returns @Just 0@ if no frames are available: this
usually means more input data needs to be provided, but if the
stretcher is running in threaded mode it may just mean that not
enough data has yet been processed.  Call 'getSamplesRequired'
to discover whether more input is needed.

This function returns @Nothing@ if all data has been fully processed
and all output read, and the stretch process is now finished.
-}
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

{- |
Obtain some processed output data from the stretcher.  Up to
the given 'Int' of samples will be in the output vectors (one per
channel for de-interleaved audio data), though it may be
less than the given number.
-}
retrieve :: Stretcher -> Int -> IO [V.Vector Float]
retrieve s samples = do
  numchans <- getChannelCount s
  ps <- replicateM numchans $ mallocArray samples
  actual <- retrieveInto s ps samples
  forM ps $ \p -> do
    fp <- newForeignPtr finalizerFree $ castPtr p
    return $ V.unsafeFromForeignPtr0 fp actual

{- |
Return the number of channels this stretcher was constructed with.
-}
getChannelCount :: Stretcher -> IO Int
getChannelCount s = withRaw s Raw.getChannelCount

{- |
Force the stretcher to calculate a stretch profile.  Normally
this happens automatically for the first 'process' call in
offline mode.

This function is provided for diagnostic purposes only.
-}
calculateStretch :: Stretcher -> IO ()
calculateStretch s = withRaw s Raw.calculateStretch

{- |
Set the level of debug output.  The value may be from 0 (errors
only) to 3 (very verbose, with audible ticks in the output at
phase reset points).  The default is whatever has been set
using 'setDefaultDebugLevel', or 0 if that function has not been
called.
-}
setDebugLevel :: Stretcher -> Int -> IO ()
setDebugLevel s n = withRaw s $ \r -> Raw.setDebugLevel r n

{- |
Set the default level of debug output for subsequently
constructed stretchers.
-}
setDefaultDebugLevel :: Int -> IO ()
setDefaultDebugLevel = Raw.setDefaultDebugLevel
