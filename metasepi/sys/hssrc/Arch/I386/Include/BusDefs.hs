module Arch.I386.Include.BusDefs where
import Foreign.C.Types
import Foreign.Ptr
import Arch.I386.Include.Types

type BusSizeT = CSize
newtype {-# CTYPE "struct bus_space_tag" #-} BusSpaceTag = BusSpaceTag ()
type BusSpaceTagT = Ptr BusSpaceTag
type BusSpaceHandleT = VaddrT
