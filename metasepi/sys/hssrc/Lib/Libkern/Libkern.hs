{-# LANGUAGE ForeignFunctionInterface #-}
module Lib.Libkern.Libkern where
import Foreign.C.Types
import Foreign.Ptr

memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
memcpy = c_memcpy

foreign import ccall "hs_extern.h memcpy"
  c_memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
  --          dest     src      size         dest
