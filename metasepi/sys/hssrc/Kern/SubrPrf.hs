{-# LANGUAGE ForeignFunctionInterface #-}
module Kern.SubrPrf where
import Foreign.C.String
import Foreign.Ptr
import Sys.Device

aprintNormalDev0 :: DeviceT -> String -> IO ()
aprintNormalDev0 dev str = withCString str $ c_aprint_normal_dev0 dev
foreign import ccall "hs_extern.h aprint_normal_dev" c_aprint_normal_dev0 ::
  DeviceT -> CString -> IO ()

printfP1 :: String -> Ptr a -> IO ()
printfP1 s p = withCString s $ (flip c_printfP1) p
foreign import ccall "hs_extern.h printf" c_printfP1 ::
  CString -> Ptr a -> IO ()
