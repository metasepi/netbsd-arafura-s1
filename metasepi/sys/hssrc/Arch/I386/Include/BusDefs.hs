module Arch.I386.Include.BusDefs where
import Foreign.C.Types
import Foreign.Ptr
import Arch.I386.Include.Types

type BusSizeT = CSize
newtype {-# CTYPE "struct bus_space_tag" #-} BusSpaceTag = BusSpaceTag ()
type BusSpaceTagT = Ptr BusSpaceTag
type BusSpaceHandleT = VaddrT
newtype {-# CTYPE "bus_dma_segment_t" #-} BusDmaSegmentT = BusDmaSegmentT ()
foreign import primitive "const.sizeof(bus_dma_segment_t)" sizeOf_BusDmaSegmentT :: Int
newtype {-# CTYPE "struct x86_bus_dmamap" #-} X86BusDmamap = X86BusDmamap ()
type BusDmamapT = Ptr X86BusDmamap
newtype {-# CTYPE "struct x86_bus_dma_tag" #-} X86BusDmaTag = X86BusDmaTag ()
type BusDmaTagT = Ptr X86BusDmaTag
