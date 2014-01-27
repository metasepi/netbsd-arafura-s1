{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich () where
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex
import Sys.Audioio
import Sys.Bus
import Dev.Pci.Auichreg
import Dev.Ic.Ac97var
import Dev.AudioIf
import Dev.Auconv

foreign export ccall "auichOpen" auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen sc flags = do
  mutexp <- p_AuichSoftc_sc_intr_lock sc
  codeif <- peek =<< p_AuichSoftc_codec_if sc
  lock <- peek =<< p_Ac97CodecIfVtbl_lock =<< peek =<< p_Ac97CodecIf_vtbl codeif
  mutexSpinExit mutexp
  call_Ac97CodecIfVtbl_lock lock codeif
  mutexSpinEnter mutexp
  return 0

foreign export ccall "auichClose" auichClose :: Ptr AuichSoftc -> IO ()
auichClose :: Ptr AuichSoftc -> IO ()
auichClose sc = do
  mutexp <- p_AuichSoftc_sc_intr_lock sc
  codeif <- peek =<< p_AuichSoftc_codec_if sc
  unlock <- peek =<< p_Ac97CodecIfVtbl_unlock =<< peek =<< p_Ac97CodecIf_vtbl codeif
  mutexSpinExit mutexp
  call_Ac97CodecIfVtbl_unlock unlock codeif
  mutexSpinEnter mutexp

foreign export ccall "auichQueryEncoding"
  auichQueryEncoding :: Ptr AuichSoftc -> Ptr AudioEncodingT -> IO Int
auichQueryEncoding :: Ptr AuichSoftc -> Ptr AudioEncodingT -> IO Int
auichQueryEncoding sc aep = do
  spdif <- peek =<< p_AuichSoftc_sc_spdif sc
  let p_ofs = if spdif then p_AuichSoftc_sc_spdif_encodings else p_AuichSoftc_sc_encodings
  encodings <- peek =<< p_ofs sc
  return =<< auconvQueryEncoding encodings aep

foreign export ccall "auichSetParams"
  auichSetParams :: Ptr AuichSoftc -> Int -> Ptr AudioParamsT -> IO Int
auichSetParams :: Ptr AuichSoftc -> Int -> Ptr AudioParamsT -> IO Int
auichSetParams sc mode p = do
  -- xxx NOT_YET_ALL
  codectype <- peek =<< p_AuichSoftc_sc_codectype sc
  when (mode == e_AudioInfoT_mode_AUMODE_PLAY && codectype == e_AC97_CODEC_TYPE_AUDIO) $ do
    iot <- peek =<< p_AuichSoftc_iot sc
    aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
    modem_offset <- peek =<< p_AuichSoftc_sc_modem_offset sc
    control <- busSpaceRead4 iot aud_ioh (e_ICH_GCTRL + modem_offset)
    channels <- peek =<< p_AudioParamsT_channels p
    scPcm246Mask <- peek =<< p_AuichSoftc_sc_pcm246_mask sc
    scPcm4 <- peek =<< p_AuichSoftc_sc_pcm4 sc
    scPcm6 <- peek =<< p_AuichSoftc_sc_pcm6 sc
    let control' = control .&. (complement scPcm246Mask) .|. (if channels == 4 then scPcm4 else scPcm6)
    busSpaceWrite4 iot aud_ioh (e_ICH_GCTRL + modem_offset) control'
  return 0


-------------------------------------------------------
-- Following code should be generated automatically. --
-------------------------------------------------------
-- Pointer type
newtype {-# CTYPE "struct auich_softc" #-} AuichSoftc = AuichSoftc ()
foreign import primitive "const.offsetof(struct auich_softc, sc_intr_lock)"
  offsetOf_AuichSoftc_sc_intr_lock :: Int
foreign import primitive "const.offsetof(struct auich_softc, codec_if)"
  offsetOf_AuichSoftc_codec_if :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_spdif)"
  offsetOf_AuichSoftc_sc_spdif :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_encodings)"
  offsetOf_AuichSoftc_sc_encodings :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_spdif_encodings)"
  offsetOf_AuichSoftc_sc_spdif_encodings :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_codectype)"
  offsetOf_AuichSoftc_sc_codectype :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_modem_offset)"
  offsetOf_AuichSoftc_sc_modem_offset :: Int
foreign import primitive "const.offsetof(struct auich_softc, iot)"
  offsetOf_AuichSoftc_iot :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_ioh)"
  offsetOf_AuichSoftc_aud_ioh :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm246_mask)"
  offsetOf_AuichSoftc_sc_pcm246_mask :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm4)"
  offsetOf_AuichSoftc_sc_pcm4 :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm6)"
  offsetOf_AuichSoftc_sc_pcm6 :: Int

-- Pointer combinator
p_AuichSoftc_sc_intr_lock :: Ptr AuichSoftc -> IO (Ptr KmutexT)
p_AuichSoftc_sc_intr_lock p = return $ plusPtr p offsetOf_AuichSoftc_sc_intr_lock
p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
p_AuichSoftc_sc_spdif :: Ptr AuichSoftc -> IO (Ptr Bool)
p_AuichSoftc_sc_spdif p = return $ plusPtr p offsetOf_AuichSoftc_sc_spdif
p_AuichSoftc_sc_encodings :: Ptr AuichSoftc -> IO (Ptr (Ptr AudioEncodingSet))
p_AuichSoftc_sc_encodings p = return $ plusPtr p offsetOf_AuichSoftc_sc_encodings
p_AuichSoftc_sc_spdif_encodings :: Ptr AuichSoftc -> IO (Ptr (Ptr AudioEncodingSet))
p_AuichSoftc_sc_spdif_encodings p = return $ plusPtr p offsetOf_AuichSoftc_sc_spdif_encodings
p_AuichSoftc_sc_codectype :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_codectype p = return $ plusPtr p offsetOf_AuichSoftc_sc_codectype
p_AuichSoftc_sc_modem_offset :: Ptr AuichSoftc -> IO (Ptr BusSizeT)
p_AuichSoftc_sc_modem_offset p = return $ plusPtr p offsetOf_AuichSoftc_sc_modem_offset
p_AuichSoftc_iot :: Ptr AuichSoftc -> IO (Ptr BusSpaceTagT)
p_AuichSoftc_iot p = return $ plusPtr p offsetOf_AuichSoftc_iot
p_AuichSoftc_aud_ioh :: Ptr AuichSoftc -> IO (Ptr BusSpaceHandleT)
p_AuichSoftc_aud_ioh p = return $ plusPtr p offsetOf_AuichSoftc_aud_ioh
p_AuichSoftc_sc_pcm246_mask :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm246_mask p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm246_mask
p_AuichSoftc_sc_pcm4 :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm4 p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm4
p_AuichSoftc_sc_pcm6 :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm6 p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm6
