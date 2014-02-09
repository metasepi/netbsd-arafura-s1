{-# LANGUAGE ForeignFunctionInterface #-}
module Arch.I386.Include.Cpu where
import Foreign.C.Types

delay :: CUInt -> IO ()
delay = c_delay
foreign import ccall "hs_extern.h delay"
  c_delay :: CUInt -> IO ()
