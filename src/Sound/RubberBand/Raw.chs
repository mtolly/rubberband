module Sound.RubberBand.Raw

( Stretcher(..)
, SampleRate, NumChannels, TimeRatio, PitchScale

, new, delete, p_delete, reset

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool)

import Sound.RubberBand.Option

#include <rubberband/rubberband-c.h>

fromOptions' :: (Integral a) => Options -> a
fromOptions' = fromIntegral . fromOptions

optionEnum' :: (Option o, Integral a) => o -> a
optionEnum' = fromIntegral . optionEnum

{#pointer RubberBandState as Stretcher newtype #}

{#context prefix="rubberband_"#}

type SampleRate = Int
type NumChannels = Int
type TimeRatio = Double
type PitchScale = Double

{#fun new as ^
  { fromIntegral `SampleRate'
  , fromIntegral `NumChannels'
  , fromOptions' `Options'
  , realToFrac `TimeRatio'
  , realToFrac `PitchScale'
  } -> `Stretcher' #}

{#fun delete as ^
  { `Stretcher' } -> `()' #}

-- | Suitable for a 'Foreign.ForeignPtr' finalizer.
foreign import ccall "&rubberband_delete"
  p_delete :: FunPtr (Ptr Stretcher -> IO ())

{#fun reset as ^
  { `Stretcher' } -> `()' #}

{#fun set_time_ratio as ^
  { `Stretcher', realToFrac `TimeRatio' } -> `()' #}
{#fun set_pitch_scale as ^
  { `Stretcher', realToFrac `PitchScale' } -> `()' #}

{#fun get_time_ratio as ^
  { `Stretcher' } -> `TimeRatio' realToFrac #}
{#fun get_pitch_scale as ^
  { `Stretcher' } -> `PitchScale' realToFrac #}

{#fun get_latency as ^
  { `Stretcher' } -> `Int' #}

{#fun set_transients_option as ^
  { `Stretcher', optionEnum' `Transients' } -> `()' #}
{#fun set_detector_option as ^
  { `Stretcher', optionEnum' `Detector' } -> `()' #}
{#fun set_phase_option as ^
  { `Stretcher', optionEnum' `Phase' } -> `()' #}
{#fun set_formant_option as ^
  { `Stretcher', optionEnum' `Formant' } -> `()' #}
{#fun set_pitch_option as ^
  { `Stretcher', optionEnum' `Pitch' } -> `()' #}

{#fun set_expected_input_duration as ^
  { `Stretcher', `Int' } -> `()' #}

{#fun get_samples_required as ^
  { `Stretcher' } -> `Int' #}

{#fun set_max_process_size as ^
  { `Stretcher', `Int' } -> `()' #}
{#fun set_key_frame_map as ^
  { `Stretcher', `Int', id `Ptr CUInt', id `Ptr CUInt' } -> `()' #}

{#fun study as ^
  { `Stretcher', id `Ptr (Ptr CFloat)', `Int', `Bool' } -> `()' #}
{#fun process as ^
  { `Stretcher', id `Ptr (Ptr CFloat)', `Int', `Bool' } -> `()' #}

{#fun available as ^
  { `Stretcher' } -> `Int' #}
{#fun retrieve as ^
  { `Stretcher', id `Ptr (Ptr CFloat)', `Int' } -> `Int' #}

{#fun get_channel_count as ^
  { `Stretcher' } -> `Int' #}

{#fun calculate_stretch as ^
  { `Stretcher' } -> `()' #}

{#fun set_debug_level as ^
  { `Stretcher', `Int' } -> `()' #}
{#fun set_default_debug_level as ^
  { `Int' } -> `()' #}
