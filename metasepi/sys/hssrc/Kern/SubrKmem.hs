{-# LANGUAGE ForeignFunctionInterface #-}
module Kern.SubrKmem where
import Foreign.C.Types
import Foreign.Ptr

type KmFlagT = CUInt

kmemAlloc = c_kmem_alloc
foreign import ccall "hs_extern.h kmem_alloc"
  c_kmem_alloc :: CSize -> KmFlagT -> IO (Ptr ())

kmemFree = c_kmem_free
foreign import ccall "hs_extern.h kmem_free"
  c_kmem_free :: Ptr () -> CSize -> IO ()

foreign import primitive "const.KM_SLEEP" e_KM_SLEEP :: KmFlagT
