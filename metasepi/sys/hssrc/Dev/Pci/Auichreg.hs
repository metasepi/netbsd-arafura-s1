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
foreign import primitive "const.ICH_PCMI" e_ICH_PCMI :: Int
foreign import primitive "const.ICH_PCMO" e_ICH_PCMO :: Int
foreign import primitive "const.ICH_DMALIST_MAX" e_ICH_DMALIST_MAX :: CSize
foreign import primitive "const.ICH_DMASEG_MAX" e_ICH_DMASEG_MAX :: CSize
foreign import primitive "const.ICH_LVI" e_ICH_LVI :: BusSizeT
foreign import primitive "const.ICH_LVI_MASK" e_ICH_LVI_MASK :: Word8
foreign import primitive "const.ICH_IOCE" e_ICH_IOCE :: Word8
foreign import primitive "const.ICH_FEIE" e_ICH_FEIE :: Word8
foreign import primitive "const.ICH_RPBM" e_ICH_RPBM :: Word8
foreign import primitive "const.ICH_DMAF_IOC" e_ICH_DMAF_IOC :: Word32
foreign import primitive "const.ICH_BDBAR" e_ICH_BDBAR :: Int
foreign import primitive "const.ICH_CIV" e_ICH_CIV :: BusSizeT
foreign import primitive "const.ICH_MICI" e_ICH_MICI :: BusSizeT
foreign import primitive "const.ICH_MINT" e_ICH_MINT :: Int
foreign import primitive "const.ICH_FIFOE" e_ICH_FIFOE :: Word16
foreign import primitive "const.ICH_BCIS" e_ICH_BCIS :: Word16
foreign import primitive "const.ICH_GSTS" e_ICH_GSTS :: BusSizeT
