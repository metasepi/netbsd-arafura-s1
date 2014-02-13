{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Device where
import Foreign.Ptr
import Foreign.C.String

newtype {-# CTYPE "struct device" #-} Device = Device ()
type DeviceT = Ptr Device
foreign import primitive "const.sizeof(struct device)"
  sizeOf_Device :: Int
foreign import primitive "const.offsetof(struct device, dv_xname)"
  offsetOf_Device_dv_xname :: Int
p_Device_dv_xname :: Ptr Device -> IO (Ptr Char)
p_Device_dv_xname p = return $ plusPtr p offsetOf_Device_dv_xname

deviceXname :: DeviceT -> IO CString
deviceXname d = fmap castPtr $ p_Device_dv_xname d
