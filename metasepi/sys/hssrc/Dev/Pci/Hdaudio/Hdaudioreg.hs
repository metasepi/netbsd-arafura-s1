{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudioreg where
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Arch.I386.Include.BusDefs

foreign import primitive "const.HDAUDIO_INTSTS_SIS_MASK" e_HDAUDIO_INTSTS_SIS_MASK :: Word32
foreign import primitive "const.HDAUDIO_MMIO_INTSTS" e_HDAUDIO_MMIO_INTSTS :: BusSizeT
foreign import primitive "const.HDAUDIO_INTSTS_CIS" e_HDAUDIO_INTSTS_CIS :: Word32
foreign import primitive "const.HDAUDIO_MMIO_RIRBSTS" e_HDAUDIO_MMIO_RIRBSTS :: BusSizeT
foreign import primitive "const.HDAUDIO_RIRBSTS_RINTFL" e_HDAUDIO_RIRBSTS_RINTFL :: Word8
foreign import primitive "const.HDAUDIO_RIRBSTS_RIRBOIS" e_HDAUDIO_RIRBSTS_RIRBOIS :: Word8
foreign import primitive "const.HDAUDIO_INTSTS_GIS" e_HDAUDIO_INTSTS_GIS :: Word32
foreign import primitive "const.HDAUDIO_MMIO_RIRBWP" e_HDAUDIO_MMIO_RIRBWP :: BusSizeT
foreign import primitive "const.HDAUDIO_RIRB_TIMEOUT" e_HDAUDIO_RIRB_TIMEOUT :: Int

data RirbEntry = RirbEntry { rirbEntry_resp    :: Word32
                           , rirbEntry_resp_ex :: Word32 }
foreign import primitive "const.sizeof(struct rirb_entry)"
  sizeOf_RirbEntry :: Int
foreign import primitive "const.offsetof(struct rirb_entry, resp)"
  offsetOf_RirbEntry_resp :: Int
foreign import primitive "const.offsetof(struct rirb_entry, resp_ex)"
  offsetOf_RirbEntry_resp_ex :: Int
instance Storable RirbEntry where
  sizeOf    = const sizeOf_RirbEntry
  alignment = sizeOf
  poke p entry = do
    pokeByteOff p offsetOf_RirbEntry_resp $ rirbEntry_resp entry
    pokeByteOff p offsetOf_RirbEntry_resp_ex $ rirbEntry_resp_ex entry
  peek p = do
    resp <- peekByteOff p offsetOf_RirbEntry_resp
    resp_ex <- peekByteOff p offsetOf_RirbEntry_resp_ex
    return $ RirbEntry { rirbEntry_resp = resp
                       , rirbEntry_resp_ex = resp_ex }

f_RIRB_CODEC_ID :: RirbEntry -> Word8
f_RIRB_CODEC_ID entry = fromIntegral $ rirbEntry_resp_ex entry .&. 0xf

f_RIRB_UNSOL :: RirbEntry -> Bool
f_RIRB_UNSOL entry = rirbEntry_resp_ex entry .&. 0x10 /= 0
