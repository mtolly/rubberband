module Sound.RubberBand.Raw where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool)

import Sound.RubberBand.Option

#include <rubberband/rubberband-c.h>

fromOptions' :: (Integral a) => Options -> a
fromOptions' = fromIntegral . fromOptions

optionEnum' :: (Option o, Integral a) => o -> a
optionEnum' = fromIntegral . optionEnum

{#pointer RubberBandState as State newtype #}

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
  } -> `State' #}

{#fun delete as ^
  { `State' } -> `()' #}

-- | Suitable for a 'Foreign.ForeignPtr' finalizer.
foreign import ccall "&rubberband_delete"
  p_delete :: FunPtr (Ptr State -> IO ())

{#fun reset as ^
  { `State' } -> `()' #}

{#fun set_time_ratio as ^
  { `State', realToFrac `TimeRatio' } -> `()' #}
{#fun set_pitch_scale as ^
  { `State', realToFrac `PitchScale' } -> `()' #}

{#fun get_time_ratio as ^
  { `State' } -> `TimeRatio' realToFrac #}
{#fun get_pitch_scale as ^
  { `State' } -> `PitchScale' realToFrac #}

{#fun get_latency as ^
  { `State' } -> `Int' #}

{#fun set_transients_option as ^
  { `State', optionEnum' `Transients' } -> `()' #}
{#fun set_detector_option as ^
  { `State', optionEnum' `Detector' } -> `()' #}
{#fun set_phase_option as ^
  { `State', optionEnum' `Phase' } -> `()' #}
{#fun set_formant_option as ^
  { `State', optionEnum' `Formant' } -> `()' #}
{#fun set_pitch_option as ^
  { `State', optionEnum' `Pitch' } -> `()' #}

{#fun set_expected_input_duration as ^
  { `State', `Int' } -> `()' #}

{#fun get_samples_required as ^
  { `State' } -> `Int' #}

{#fun set_max_process_size as ^
  { `State', `Int' } -> `()' #}
{#fun set_key_frame_map as ^
  { `State', `Int', id `Ptr CUInt', id `Ptr CUInt' } -> `()' #}

{#fun study as ^
  { `State', id `Ptr (Ptr CFloat)', `Int', `Bool' } -> `()' #}
{#fun process as ^
  { `State', id `Ptr (Ptr CFloat)', `Int', `Bool' } -> `()' #}

{#fun available as ^
  { `State' } -> `Int' #}
{#fun retrieve as ^
  { `State', id `Ptr (Ptr CFloat)', `Int' } -> `Int' #}

{#fun get_channel_count as ^
  { `State' } -> `Int' #}

{#fun calculate_stretch as ^
  { `State' } -> `()' #}

{#fun set_debug_level as ^
  { `State', `Int' } -> `()' #}
{#fun set_default_debug_level as ^
  { `Int' } -> `()' #}
