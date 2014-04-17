module Sound.RubberBand.Option where

import Data.Bits
import Data.Maybe (fromMaybe)

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

data Process    = Offline      | RealTime                      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Stretch    = Elastic      | Precise                       deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Transients = Crisp        | Mixed       | Smooth          deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Detector   = Compound     | Percussive  | Soft            deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Phase      = Laminar      | Independent                   deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Threading  = Auto         | Never       | Always          deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Window     = Standard     | Short       | Long            deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Smoothing  = SmoothingOff | SmoothingOn                   deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Formant    = Shifted      | Preserved                     deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Pitch      = HighSpeed    | HighQuality | HighConsistency deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Channels   = Apart        | Together                      deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

defaultOptions :: Options
defaultOptions = toOptions 0x00000000

percussiveOptions :: Options
percussiveOptions = toOptions 0x00102000
  