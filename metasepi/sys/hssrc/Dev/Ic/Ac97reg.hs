{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Ic.Ac97reg where
import Foreign.Ptr

foreign import primitive "const.AC97_REG_LINE1_RATE" e_AC97_REG_LINE1_RATE :: Word8
foreign import primitive "const.AC97_REG_LINE1_LEVEL" e_AC97_REG_LINE1_LEVEL :: Word8

foreign import primitive "const.AC97_REG_PCM_LR_ADC_RATE" e_AC97_REG_PCM_LR_ADC_RATE :: Int
foreign import primitive "const.AC97_REG_PCM_FRONT_DAC_RATE" e_AC97_REG_PCM_FRONT_DAC_RATE :: Int
foreign import primitive "const.AC97_REG_PCM_SURR_DAC_RATE" e_AC97_REG_PCM_SURR_DAC_RATE :: Int
foreign import primitive "const.AC97_REG_PCM_LFE_DAC_RATE" e_AC97_REG_PCM_LFE_DAC_RATE :: Int

foreign import primitive "const.AC97_EXT_AUDIO_VRA" e_AC97_EXT_AUDIO_VRA :: Int
