module Arch.I386.I386.BusDma where
import Foreign.C.Types
import Foreign.Ptr
import Sys.Types
import Arch.I386.Include.Types
import Arch.I386.Include.BusDefs

busDmamemMmap = c_bus_dmamem_mmap
foreign import ccall "hs_extern.h bus_dmamem_mmap"
  c_bus_dmamem_mmap :: BusDmaTagT -> Ptr BusDmaSegmentT -> Int -> OffT -> Int -> Int -> IO PaddrT
