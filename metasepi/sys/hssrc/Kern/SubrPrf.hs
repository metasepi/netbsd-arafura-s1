{-# LANGUAGE ForeignFunctionInterface #-}
module Kern.SubrPrf where
import Foreign.C.String
import Foreign.Ptr
import Sys.Device

aprintNormalDev0 :: DeviceT -> String -> IO ()
aprintNormalDev0 dev str = withCString str $ c_aprint_normal_dev0 dev
foreign import ccall "hs_extern.h aprint_normal_dev" c_aprint_normal_dev0 ::
  DeviceT -> CString -> IO ()

aprintErrorDev0 :: DeviceT -> String -> IO ()
aprintErrorDev0 dev str = withCString str $ c_aprint_error_dev0 dev
foreign import ccall "hs_extern.h aprint_error_dev" c_aprint_error_dev0 ::
  DeviceT -> CString -> IO ()

printfP1 :: String -> Ptr a -> IO ()
printfP1 s p = withCString s $ (flip c_printfP1) p
foreign import ccall "hs_extern.h printf" c_printfP1 ::
  CString -> Ptr a -> IO ()

printfS1 :: String -> CString -> IO ()
printfS1 s cs = withCString s $ (flip c_printfP1) cs
foreign import ccall "hs_extern.h printf" c_printfS1 ::
  CString -> CString -> IO ()
