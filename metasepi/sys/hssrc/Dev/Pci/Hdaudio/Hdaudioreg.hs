{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudioreg where
import Data.Word
import Arch.I386.Include.BusDefs

foreign import primitive "const.HDAUDIO_INTSTS_SIS_MASK" e_HDAUDIO_INTSTS_SIS_MASK :: Word32
foreign import primitive "const.HDAUDIO_MMIO_INTSTS" e_HDAUDIO_MMIO_INTSTS :: BusSizeT
foreign import primitive "const.HDAUDIO_INTSTS_CIS" e_HDAUDIO_INTSTS_CIS :: Word32
foreign import primitive "const.HDAUDIO_MMIO_RIRBSTS" e_HDAUDIO_MMIO_RIRBSTS :: BusSizeT
foreign import primitive "const.HDAUDIO_RIRBSTS_RINTFL" e_HDAUDIO_RIRBSTS_RINTFL :: Word8
foreign import primitive "const.HDAUDIO_RIRBSTS_RIRBOIS" e_HDAUDIO_RIRBSTS_RIRBOIS :: Word8
foreign import primitive "const.HDAUDIO_INTSTS_GIS" e_HDAUDIO_INTSTS_GIS :: Word32
