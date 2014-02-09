{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Device where
import Foreign.Ptr

newtype {-# CTYPE "struct device" #-} Device = Device ()
type DeviceT = Ptr Device
foreign import primitive "const.sizeof(struct device)"
  sizeOf_Device :: Int
