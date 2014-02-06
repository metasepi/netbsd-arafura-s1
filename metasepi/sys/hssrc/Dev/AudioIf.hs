{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.AudioIf where
import Foreign.C.Types
import Foreign.Ptr

newtype {-# CTYPE "struct audio_params" #-} AudioParams = AudioParams ()
type AudioParamsT = AudioParams

foreign import primitive "const.offsetof(struct audio_params, channels)"
  offsetOf_AudioParamsT_channels :: Int
p_AudioParamsT_channels :: Ptr AudioParamsT -> IO (Ptr CUInt)
p_AudioParamsT_channels p = return $ plusPtr p offsetOf_AudioParamsT_channels

foreign import primitive "const.offsetof(struct audio_params, sample_rate)"
  offsetOf_AudioParams_sample_rate :: Int
p_AudioParams_sample_rate :: Ptr AudioParams -> IO (Ptr CUInt)
p_AudioParams_sample_rate p = return $ plusPtr p $ offsetOf_AudioParams_sample_rate
