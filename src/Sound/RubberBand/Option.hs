module Sound.RubberBand.Option where

import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (fromMaybe)

{- |
Processing options for the timestretcher. The preferred
options should normally be set when calling 'Sound.RubberBand.Nice.new'.
-}
data Options = Options
  { oProcess    :: Process
  , oStretch    :: Stretch
  , oTransients :: Transients
  , oDetector   :: Detector
  , oPhase      :: Phase
  , oThreading  :: Threading
  , oWindow     :: Window
  , oSmoothing  :: Smoothing
  , oFormant    :: Formant
  , oPitch      :: Pitch
  , oChannels   :: Channels
  } deriving (Eq, Ord, Show, Read)

{- |
'Process' flags determine how the timestretcher
will be invoked. These options may not be changed after
construction.

The 'Process' setting is likely to depend on your architecture:
non-real-time operation on seekable files: 'Offline'; real-time
or streaming operation: 'RealTime'.
-}
data Process
  = Offline
  {- ^
  Run the stretcher in offline mode. In this mode the input data needs to
  be provided twice, once to 'Sound.RubberBand.Nice.study', which calculates
  a stretch profile for the audio, and once to
  'Sound.RubberBand.Nice.process', which stretches it.
  -}
  | RealTime
  {- ^
  Run the stretcher in real-time mode. In this mode only
  'Sound.RubberBand.Nice.process' should be called, and the
  stretcher adjusts dynamically in response to the input audio.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Stretch' flags control the profile used for
variable timestretching.  Rubber Band always adjusts the
stretch profile to minimise stretching of busy broadband
transient sounds, but the degree to which it does so is
adjustable.  These options may not be changed after
construction.
-}
data Stretch
  = Elastic
  {- ^
  Only meaningful in offline
  mode, and the default in that mode.  The audio will be
  stretched at a variable rate, aimed at preserving the quality
  of transient sounds as much as possible.  The timings of low
  activity regions between transients may be less exact than
  when the precise flag is set.
  -}
  | Precise
  {- ^
  Although still using a variable
  stretch rate, the audio will be stretched so as to maintain
  as close as possible to a linear stretch ratio throughout.
  Timing may be better than when using 'Elastic', at
  slight cost to the sound quality of transients.  This setting
  is always used when running in real-time mode.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Transients' flags control the component
frequency phase-reset mechanism that may be used at transient
points to provide clarity and realism to percussion and other
significant transient sounds.  These options may be changed
after construction when running in real-time mode, but not when
running in offline mode.
-}
data Transients
  = Crisp
  {- ^
  Reset component phases at the
  peak of each transient (the start of a significant note or
  percussive event).  This, the default setting, usually
  results in a clear-sounding output; but it is not always
  consistent, and may cause interruptions in stable sounds
  present at the same time as transient events.  The
  'Detector' flags can be used to tune this to some
  extent.
  -}
  | Mixed
  {- ^
  Reset component phases at the
  peak of each transient, outside a frequency range typical of
  musical fundamental frequencies.  The results may be more
  regular for mixed stable and percussive notes than
  'Crisp', but with a "phasier" sound.  The
  balance may sound very good for certain types of music and
  fairly bad for others.
  -}
  | Smooth
  {- ^
  Do not reset component phases
  at any point.  The results will be smoother and more regular
  but may be less clear than with either of the other
  'Transients' flags.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Detector' flags control the type of
transient detector used.  These options may be changed
after construction when running in real-time mode, but not when
running in offline mode.
-}
data Detector
  = Compound
  {- ^
  Use a general-purpose
  transient detector which is likely to be good for most
  situations.  This is the default.
  -}
  | Percussive
  {- ^
  Detect percussive
  transients.  Note that this was the default and only option
  in Rubber Band versions prior to 1.5.
  -}
  | Soft
  {- ^
  Use an onset detector with less
  of a bias toward percussive transients.  This may give better
  results with certain material (e.g. relatively monophonic
  piano music).
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Phase' flags control the adjustment of
component frequency phases from one analysis window to the next
during non-transient segments.  These options may be changed at
any time.
-}
data Phase
  = Laminar
  {- ^
  Adjust phases when stretching in
  such a way as to try to retain the continuity of phase
  relationships between adjacent frequency bins whose phases
  are behaving in similar ways.  This, the default setting,
  should give good results in most situations.
  -}
  | Independent
  {- ^
  Adjust the phase in each
  frequency bin independently from its neighbours.  This
  usually results in a slightly softer, phasier sound.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Threading' flags control the threading
model of the stretcher.  These options may not be changed after
construction.
-}
data Threading
  = Auto
  {- ^
  Permit the stretcher to
  determine its own threading model.  Usually this means using
  one processing thread per audio channel in offline mode if
  the stretcher is able to determine that more than one CPU is
  available, and one thread only in realtime mode.  This is the
  defafult.
  -}
  | Never
  {- ^
  Never use more than one thread.
  -}
  | Always
  {- ^
  Use multiple threads in any
  situation where 'Auto' would do so, except omit
  the check for multiple CPUs and instead assume it to be true.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Window' flags control the window size for
FFT processing.  The window size actually used will depend on
many factors, but it can be influenced.  These options may not
be changed after construction.
-}
data Window
  = Standard
  {- ^
  Use the default window size.
  The actual size will vary depending on other parameters.
  This option is expected to produce better results than the
  other window options in most situations.
  -}
  | Short
  {- ^
  Use a shorter window.  This may
  result in crisper sound for audio that depends strongly on
  its timing qualities.
  -}
  | Long
  {- ^
  Use a longer window.  This is
  likely to result in a smoother sound at the expense of
  clarity and timing.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Smoothing' flags control the use of
window-presum FFT and time-domain smoothing.  These options may
not be changed after construction.
-}
data Smoothing
  = SmoothingOff
  {- ^
  Do not use time-domain smoothing.
  This is the default.
  -}
  | SmoothingOn
  {- ^
  Use time-domain smoothing.  This
  will result in a softer sound with some audible artifacts
  around sharp transients, but it may be appropriate for longer
  stretches of some instruments and can mix well with a 'Window' setting of
  'Short'.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Formant' flags control the handling of
formant shape (spectral envelope) when pitch-shifting.  These
options may be changed at any time.
-}
data Formant
  = Shifted
  {- ^
  Apply no special formant
  processing.  The spectral envelope will be pitch shifted as
  normal.  This is the default.
  -}
  | Preserved
  {- ^
  Preserve the spectral
  envelope of the unshifted signal.  This permits shifting the
  note frequency without so substantially affecting the
  perceived pitch profile of the voice or instrument.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Pitch' flags control the method used for
pitch shifting.  These options may be changed at any time.
They are only effective in realtime mode; in offline mode, the
pitch-shift method is fixed.
-}
data Pitch
  = HighSpeed
  {- ^
  Use a method with a CPU cost
  that is relatively moderate and predictable.  This may
  sound less clear than 'HighQuality', especially
  for large pitch shifts.  This is the default.
  -}
  | HighQuality
  {- ^
  Use the highest quality
  method for pitch shifting.  This method has a CPU cost
  approximately proportional to the required frequency shift.
  -}
  | HighConsistency
  {- ^
  Use the method that gives
  greatest consistency when used to create small variations in
  pitch around the 1.0-ratio level.  Unlike the previous two
  options, this avoids discontinuities when moving across the
  1.0 pitch scale in real-time; it also consumes more CPU than
  the others in the case where the pitch scale is exactly 1.0.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- |
'Channels' flags control the method used for
processing two-channel audio.  These options may not be changed
after construction.
-}
data Channels
  = Apart
  {- ^
  Each channel is processed
  individually, though timing is synchronised and phases are
  synchronised at transients (depending on the 'Transients'
  setting).  This gives the highest quality for the individual
  channels but a relative lack of stereo focus and unrealistic
  increase in "width".  This is the default.
  -}
  | Together
  {- ^
  The first two channels (where
  two or more are present) are considered to be a stereo pair
  and are processed in mid-side format; mid and side are
  processed individually, with timing synchronised and phases
  synchronised at transients (depending on the 'Transients'
  setting).  This usually leads to better focus in the centre
  but a loss of stereo space and width.  Any channels beyond
  the first two are processed individually.
  -}
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

class (Enum o, Bounded o) => Option o where
  optionEnum :: o -> Int

setOption :: (Option o) => o -> Int -> Int
setOption o i = let
  allOptions = [minBound..maxBound] `asTypeOf` [o]
  mask = foldr (.|.) 0 $ map optionEnum allOptions
  in (i .&. complement mask) .|. optionEnum o

getOption :: (Option o) => Int -> o
getOption i = let
  allOptions = [minBound..maxBound] `asTypeOf` [o]
  lookupList = zip (map optionEnum allOptions) allOptions
  mask = foldr (.|.) 0 $ map fst lookupList
  masked = i .&. mask
  o = fromMaybe
    (error $ "getOption: no result for masked value " ++ show masked)
    (lookup masked lookupList)
  in o

toOptions :: Int -> Options
toOptions i = Options
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)
  (getOption i)

fromOptions :: Options -> Int
fromOptions (Options a b c d e f g h i j k) = foldr (.|.) 0
  [ optionEnum a
  , optionEnum b
  , optionEnum c
  , optionEnum d
  , optionEnum e
  , optionEnum f
  , optionEnum g
  , optionEnum h
  , optionEnum i
  , optionEnum j
  , optionEnum k
  ]

instance Option Process where
  optionEnum Offline  = 0x00000000
  optionEnum RealTime = 0x00000001

instance Option Stretch where
  optionEnum Elastic = 0x00000000
  optionEnum Precise = 0x00000010

instance Option Transients where
  optionEnum Crisp  = 0x00000000
  optionEnum Mixed  = 0x00000100
  optionEnum Smooth = 0x00000200

instance Option Detector where
  optionEnum Compound   = 0x00000000
  optionEnum Percussive = 0x00000400
  optionEnum Soft       = 0x00000800

instance Option Phase where
  optionEnum Laminar     = 0x00000000
  optionEnum Independent = 0x00002000

instance Option Threading where
  optionEnum Auto   = 0x00000000
  optionEnum Never  = 0x00010000
  optionEnum Always = 0x00020000

instance Option Window where
  optionEnum Standard = 0x00000000
  optionEnum Short    = 0x00100000
  optionEnum Long     = 0x00200000

instance Option Smoothing where
  optionEnum SmoothingOff = 0x00000000
  optionEnum SmoothingOn  = 0x00800000

instance Option Formant where
  optionEnum Shifted   = 0x00000000
  optionEnum Preserved = 0x01000000

instance Option Pitch where
  optionEnum HighSpeed       = 0x00000000
  optionEnum HighQuality     = 0x02000000
  optionEnum HighConsistency = 0x04000000

instance Option Channels where
  optionEnum Apart    = 0x00000000
  optionEnum Together = 0x10000000

-- | Intended to give good results in most situations.
defaultOptions :: Options
defaultOptions = toOptions 0x00000000

percussiveOptions :: Options
percussiveOptions = toOptions 0x00102000
  