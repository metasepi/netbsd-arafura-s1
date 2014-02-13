{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Device where
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

foreign import primitive "const.DVF_ACTIVE" e_DVF_ACTIVE :: Int
foreign import primitive "const.DVF_BUS_SUSPENDED" e_DVF_BUS_SUSPENDED :: Int

newtype {-# CTYPE "struct device" #-} Device = Device ()
type DeviceT = Ptr Device
foreign import primitive "const.sizeof(struct device)"
  sizeOf_Device :: Int
foreign import primitive "const.offsetof(struct device, dv_xname)"
  offsetOf_Device_dv_xname :: Int
p_Device_dv_xname :: Ptr Device -> IO (Ptr Char)
p_Device_dv_xname p = return $ plusPtr p offsetOf_Device_dv_xname
foreign import primitive "const.offsetof(struct device, dv_flags)"
  offsetOf_Device_dv_flags :: Int
p_Device_dv_flags :: Ptr Device -> IO (Ptr Int)
p_Device_dv_flags p = return $ plusPtr p $ offsetOf_Device_dv_flags

deviceXname :: DeviceT -> IO CString
deviceXname d = fmap castPtr $ p_Device_dv_xname d

deviceHasPower :: DeviceT -> IO Bool
deviceHasPower d = do
  f <- peek =<< p_Device_dv_flags d
  return $ f .&. (e_DVF_ACTIVE .|. e_DVF_BUS_SUSPENDED) == e_DVF_ACTIVE
