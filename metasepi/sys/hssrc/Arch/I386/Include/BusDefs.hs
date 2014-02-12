module Arch.I386.Include.BusDefs where
import Foreign.C.Types
import Foreign.Ptr
import Arch.I386.Include.Types

type BusAddrT = PaddrT
type BusSizeT = CSize
newtype {-# CTYPE "struct bus_space_tag" #-} BusSpaceTag = BusSpaceTag ()
type BusSpaceTagT = Ptr BusSpaceTag
type BusSpaceHandleT = VaddrT

newtype {-# CTYPE "struct x86_bus_dma_segment" #-} X86BusDmaSegment = X86BusDmaSegment ()
type BusDmaSegmentT = X86BusDmaSegment
foreign import primitive "const.sizeof(struct x86_bus_dma_segment)"
  sizeOf_X86BusDmaSegment :: Int
sizeOf_BusDmaSegmentT = sizeOf_X86BusDmaSegment
foreign import primitive "const.offsetof(struct x86_bus_dma_segment, ds_addr)"
  offsetOf_X86BusDmaSegment_ds_addr :: Int
p_X86BusDmaSegment_ds_addr :: Ptr X86BusDmaSegment -> IO (Ptr BusAddrT)
p_X86BusDmaSegment_ds_addr p = return $ plusPtr p $ offsetOf_X86BusDmaSegment_ds_addr
p_BusDmaSegment_ds_addr = p_X86BusDmaSegment_ds_addr

newtype {-# CTYPE "struct x86_bus_dmamap" #-} X86BusDmamap = X86BusDmamap ()
type BusDmamapT = Ptr X86BusDmamap
foreign import primitive "const.sizeof(struct x86_bus_dmamap)"
  sizeOf_X86BusDmamap :: Int
foreign import primitive "const.offsetof(struct x86_bus_dmamap, dm_segs)"
  offsetOf_X86BusDmamap_dm_segs :: Int
p_X86BusDmamap_dm_segs :: Ptr X86BusDmamap -> Int -> IO (Ptr BusDmaSegmentT)
p_X86BusDmamap_dm_segs p i = return $ plusPtr p $ offsetOf_X86BusDmamap_dm_segs + i * sizeOf_X86BusDmaSegment
p_BusDmamap_dm_segs = p_X86BusDmamap_dm_segs

newtype {-# CTYPE "struct x86_bus_dma_tag" #-} X86BusDmaTag = X86BusDmaTag ()
type BusDmaTagT = Ptr X86BusDmaTag
