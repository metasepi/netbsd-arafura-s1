{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich where
import Foreign.Ptr

foreign export ccall "auichOpen" auichOpen :: Ptr () -> Int -> IO Int
auichOpen :: Ptr () -> Int -> IO Int
auichOpen addr flags = return 0
