{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Bus (module Sys.Bus, module Arch.I386.Include.BusDefs, module Arch.I386.I386.BusDma) where
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Sys.Proc
import Arch.I386.Include.BusDefs
import Arch.I386.I386.BusDma

foreign import primitive "const.BUS_DMA_WAITOK" e_BUS_DMA_WAITOK :: Int
foreign import primitive "const.BUS_DMASYNC_POSTREAD" e_BUS_DMASYNC_POSTREAD :: Int
foreign import primitive "const.BUS_DMASYNC_PREREAD" e_BUS_DMASYNC_PREREAD :: Int

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

busDmamemAlloc = c_bus_dmamem_alloc
busDmamemFree = c_bus_dmamem_free
foreign import ccall "hs_extern.h bus_dmamem_alloc" c_bus_dmamem_alloc ::
  BusDmaTagT -> BusSizeT -> BusSizeT -> BusSizeT -> Ptr BusDmaSegmentT -> Int -> Ptr Int -> Int -> IO Int
foreign import ccall "hs_extern.h bus_dmamem_free" c_bus_dmamem_free ::
  BusDmaTagT -> Ptr BusDmaSegmentT -> Int -> IO ()

busDmamemMap = c_bus_dmamem_map
busDmamemUnmap = c_bus_dmamem_unmap
foreign import ccall "hs_extern.h bus_dmamem_map" c_bus_dmamem_map ::
  BusDmaTagT -> Ptr BusDmaSegmentT -> Int -> CSize -> Ptr (Ptr ()) -> Int -> IO Int
foreign import ccall "hs_extern.h bus_dmamem_unmap" c_bus_dmamem_unmap ::
  BusDmaTagT -> Ptr () -> CSize -> IO ()

busDmamapCreate = c_bus_dmamap_create
busDmamapDestroy = c_bus_dmamap_destroy
foreign import ccall "hs_extern.h bus_dmamap_create" c_bus_dmamap_create ::
  BusDmaTagT -> BusSizeT -> Int -> BusSizeT -> BusSizeT -> Int -> Ptr BusDmamapT -> IO Int
foreign import ccall "hs_extern.h bus_dmamap_destroy" c_bus_dmamap_destroy ::
  BusDmaTagT -> BusDmamapT -> IO ()

busDmamapLoad = c_bus_dmamap_load
busDmamapUnload = c_bus_dmamap_unload
foreign import ccall "hs_extern.h bus_dmamap_load" c_bus_dmamap_load ::
  BusDmaTagT -> BusDmamapT -> Ptr () -> BusSizeT -> Ptr Proc -> Int -> IO Int
foreign import ccall "hs_extern.h bus_dmamap_unload" c_bus_dmamap_unload ::
  BusDmaTagT -> BusDmamapT -> IO ()

busDmamapSync = c_bus_dmamap_sync
foreign import ccall "hs_extern.h bus_dmamap_sync" c_bus_dmamap_sync ::
  BusDmaTagT -> BusDmamapT -> BusAddrT -> BusSizeT -> Int -> IO ()
