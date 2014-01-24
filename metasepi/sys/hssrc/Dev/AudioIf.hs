{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.AudioIf where
import Foreign.C.Types
import Foreign.Ptr

newtype {-# CTYPE "audio_params_t" #-} AudioParamsT = AudioParamsT ()

foreign import primitive "const.offsetof(struct audio_params, channels)"
  offsetOf_AudioParamsT_channels :: Int
p_AudioParamsT_channels :: Ptr AudioParamsT -> IO (Ptr CUInt)
p_AudioParamsT_channels p = return $ plusPtr p offsetOf_AudioParamsT_channels
