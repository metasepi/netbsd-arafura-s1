{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudioreg where
import Data.Word
import Arch.I386.Include.BusDefs

foreign import primitive "const.HDAUDIO_INTSTS_SIS_MASK" e_HDAUDIO_INTSTS_SIS_MASK :: Word32
foreign import primitive "const.HDAUDIO_MMIO_INTSTS" e_HDAUDIO_MMIO_INTSTS :: BusSizeT
