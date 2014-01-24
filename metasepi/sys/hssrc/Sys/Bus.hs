{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Bus (busSpaceRead4, busSpaceWrite4, BusSpaceTagT, BusSpaceHandleT, BusSizeT) where
import Data.Word
import Foreign.Ptr
import Arch.I386.Include.BusDefs

busSpaceRead4 = c_bus_space_read_4
busSpaceWrite4 = c_bus_space_write_4

foreign import ccall "hs_extern.h bus_space_read_4" c_bus_space_read_4 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> IO Word32
foreign import ccall "hs_extern.h bus_space_write_4" c_bus_space_write_4 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> Word32 -> IO ()
