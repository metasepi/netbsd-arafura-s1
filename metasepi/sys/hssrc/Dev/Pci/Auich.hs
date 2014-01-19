{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich () where
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex

foreign export ccall "auichOpen" auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen addr flags = do
  let mutexp = p_AuichSoftc_sc_intr_lock addr
  c_mutex_spin_enter $ mutexp
  return 0


-------------------------------------------------------
-- Following code should be generated automatically. --
-------------------------------------------------------

-- Pointer type
newtype {-# CTYPE "struct auich_softc" #-} AuichSoftc = AuichSoftc ()
foreign import primitive "const.offsetof(struct auich_softc, sc_intr_lock)"
  offsetOf_AuichSoftc_sc_intr_lock :: Int

-- Pointer combinator
p_AuichSoftc_sc_intr_lock :: Ptr AuichSoftc -> Ptr KmutexT
p_AuichSoftc_sc_intr_lock p =
  plusPtr p offsetOf_AuichSoftc_sc_intr_lock
