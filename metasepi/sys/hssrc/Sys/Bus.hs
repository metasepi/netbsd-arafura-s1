{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Bus (busSpaceRead4, busSpaceWrite4, busSpaceRead2, busSpaceWrite2, busSpaceRead1, busSpaceWrite1,
                BusSpaceTagT, BusSpaceHandleT, BusSizeT) where
import Data.Word
import Foreign.Ptr
import Arch.I386.Include.BusDefs

busSpaceRead4 = c_bus_space_read_4
busSpaceWrite4 = c_bus_space_write_4
foreign import ccall "hs_extern.h bus_space_read_4" c_bus_space_read_4 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> IO Word32
foreign import ccall "hs_extern.h bus_space_write_4" c_bus_space_write_4 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> Word32 -> IO ()

busSpaceRead2 = c_bus_space_read_2
busSpaceWrite2 = c_bus_space_write_2
foreign import ccall "hs_extern.h bus_space_read_2" c_bus_space_read_2 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> IO Word16
foreign import ccall "hs_extern.h bus_space_write_2" c_bus_space_write_2 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> Word16 -> IO ()

busSpaceRead1 = c_bus_space_read_1
busSpaceWrite1 = c_bus_space_write_1
foreign import ccall "hs_extern.h bus_space_read_1" c_bus_space_read_1 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> IO Word8
foreign import ccall "hs_extern.h bus_space_write_1" c_bus_space_write_1 ::
  BusSpaceTagT -> BusSpaceHandleT -> BusSizeT -> Word8 -> IO ()
