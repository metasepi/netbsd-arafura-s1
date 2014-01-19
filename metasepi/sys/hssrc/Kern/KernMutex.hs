{-# LANGUAGE ForeignFunctionInterface #-}
module Kern.KernMutex where
import Foreign.Ptr

newtype {-# CTYPE "kmutex_t" #-} KmutexT = KmutexT ()

mutexSpinEnter, mutexSpinExit :: Ptr KmutexT -> IO ()
mutexSpinEnter = c_mutex_spin_enter
mutexSpinExit = c_mutex_spin_exit
foreign import ccall "hs_extern.h mutex_spin_enter" c_mutex_spin_enter :: Ptr KmutexT -> IO ()
foreign import ccall "hs_extern.h mutex_spin_exit" c_mutex_spin_exit :: Ptr KmutexT -> IO ()
