{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Ic.Ac97reg where
import Foreign.Ptr

foreign import primitive "const.AC97_REG_LINE1_RATE" e_AC97_REG_LINE1_RATE :: Word8
foreign import primitive "const.AC97_REG_LINE1_LEVEL" e_AC97_REG_LINE1_LEVEL :: Word8
