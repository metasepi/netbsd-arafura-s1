{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auichreg where
import Foreign.C.Types
import Sys.Bus

foreign import primitive "const.ICH_GCTRL" e_ICH_GCTRL :: BusSizeT
foreign import primitive "const.ICH_SEMATIMO" e_ICH_SEMATIMO :: CUInt
foreign import primitive "const.ICH_CAS" e_ICH_CAS :: BusSizeT
foreign import primitive "const.ICH_CODEC_OFFSET" e_ICH_CODEC_OFFSET :: BusSizeT
foreign import primitive "const.ICH_CTRL" e_ICH_CTRL :: BusSizeT
foreign import primitive "const.ICH_STS" e_ICH_STS :: BusSizeT
foreign import primitive "const.ICH_DCH" e_ICH_DCH :: Word32
foreign import primitive "const.ICH_RR" e_ICH_RR :: Word8
