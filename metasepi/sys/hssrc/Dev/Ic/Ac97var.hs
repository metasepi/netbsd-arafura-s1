{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Ic.Ac97var where
import Foreign.Ptr

foreign import primitive "const.AC97_CODEC_TYPE_AUDIO" e_AC97_CODEC_TYPE_AUDIO :: Int
foreign import primitive "const.AC97_CODEC_TYPE_MODEM" e_AC97_CODEC_TYPE_MODEM :: Int
