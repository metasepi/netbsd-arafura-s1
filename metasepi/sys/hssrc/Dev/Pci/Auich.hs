{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich () where
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex
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

newtype {-# CTYPE "struct ac97_codec_if" #-} Ac97CodecIf = Ac97CodecIf ()
foreign import primitive "const.offsetof(struct ac97_codec_if, vtbl)"
  offsetOf_Ac97CodecIf_vtbl :: Int

newtype {-# CTYPE "struct ac97_codec_if_vtbl" #-} Ac97CodecIfVtbl = Ac97CodecIfVtbl ()
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, lock)"
  offsetOf_Ac97CodecIfVtbl_lock :: Int
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, unlock)"
  offsetOf_Ac97CodecIfVtbl_unlock :: Int
type Ac97CodecIfVtbl_lock = Ptr Ac97CodecIf -> IO (Ptr ())
type Ac97CodecIfVtbl_unlock = Ptr Ac97CodecIf -> IO (Ptr ())
foreign import ccall "hs_extern.h funptr_apply_p1" call_Ac97CodecIfVtbl_lock ::
  FunPtr (Ac97CodecIfVtbl_lock) -> Ptr Ac97CodecIf -> IO ()
foreign import ccall "hs_extern.h funptr_apply_p1" call_Ac97CodecIfVtbl_unlock ::
  FunPtr (Ac97CodecIfVtbl_unlock) -> Ptr Ac97CodecIf -> IO ()

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

p_Ac97CodecIf_vtbl :: Ptr Ac97CodecIf -> IO (Ptr (Ptr Ac97CodecIfVtbl))
p_Ac97CodecIf_vtbl p = return $ plusPtr p offsetOf_Ac97CodecIf_vtbl
p_Ac97CodecIfVtbl_lock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_lock))
p_Ac97CodecIfVtbl_lock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_lock
p_Ac97CodecIfVtbl_unlock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_unlock))
p_Ac97CodecIfVtbl_unlock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_unlock
