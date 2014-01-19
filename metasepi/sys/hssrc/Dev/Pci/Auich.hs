{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich () where
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex

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

-------------------------------------------------------
-- Following code should be generated automatically. --
-------------------------------------------------------
-- Pointer type
newtype {-# CTYPE "struct auich_softc" #-} AuichSoftc = AuichSoftc ()
foreign import primitive "const.offsetof(struct auich_softc, sc_intr_lock)"
  offsetOf_AuichSoftc_sc_intr_lock :: Int
foreign import primitive "const.offsetof(struct auich_softc, codec_if)"
  offsetOf_AuichSoftc_codec_if :: Int
newtype {-# CTYPE "struct ac97_codec_if" #-} Ac97CodecIf = Ac97CodecIf ()
foreign import primitive "const.offsetof(struct ac97_codec_if, vtbl)"
  offsetOf_Ac97CodecIf_vtbl :: Int
newtype {-# CTYPE "struct ac97_codec_if_vtbl" #-} Ac97CodecIfVtbl = Ac97CodecIfVtbl ()
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, lock)"
  offsetOf_Ac97CodecIfVtbl_lock :: Int
type Ac97CodecIfVtbl_lock = Ptr Ac97CodecIf -> IO (Ptr ())
foreign import ccall "funptr_apply_p1" call_Ac97CodecIfVtbl_lock ::
  FunPtr (Ac97CodecIfVtbl_lock) -> Ptr Ac97CodecIf -> IO ()

-- Pointer combinator
p_AuichSoftc_sc_intr_lock :: Ptr AuichSoftc -> IO (Ptr KmutexT)
p_AuichSoftc_sc_intr_lock p = return $ plusPtr p offsetOf_AuichSoftc_sc_intr_lock
p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
p_Ac97CodecIf_vtbl :: Ptr Ac97CodecIf -> IO (Ptr (Ptr Ac97CodecIfVtbl))
p_Ac97CodecIf_vtbl p = return $ plusPtr p offsetOf_Ac97CodecIf_vtbl
p_Ac97CodecIfVtbl_lock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_lock))
p_Ac97CodecIfVtbl_lock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_lock
