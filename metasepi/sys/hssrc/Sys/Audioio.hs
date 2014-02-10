{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Audioio where
import Foreign.Ptr

foreign import primitive "const.AUMODE_PLAY" e_AudioInfoT_mode_AUMODE_PLAY :: Int
foreign import primitive "const.AUMODE_RECORD" e_AudioInfoT_mode_AUMODE_RECORD :: Int
foreign import primitive "const.AUMODE_PLAY_ALL" e_AudioInfoT_mode_AUMODE_PLAY_ALL :: Int

newtype {-# CTYPE "struct audio_device" #-} AudioDevice = AudioDevice ()
type AudioDeviceT = AudioDevice
foreign import primitive "const.sizeof(struct audio_device)"
  sizeOf_AudioDevice :: Int
