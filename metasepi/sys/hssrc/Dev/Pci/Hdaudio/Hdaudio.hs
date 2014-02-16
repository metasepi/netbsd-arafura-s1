{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudio () where
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex
import Dev.Pci.Hdaudio.Hdaudioreg
import Dev.Pci.Hdaudio.Hdaudiovar

foreign export ccall "hdaudioIntr" hdaudioIntr :: Ptr HdaudioSoftc -> Word32 -> IO Int
hdaudioIntr :: Ptr HdaudioSoftc -> Word32 -> IO Int
hdaudioIntr sc intsts = do
  -- xxx Not yet all
  when (intsts .&. e_HDAUDIO_INTSTS_SIS_MASK /= 0) $ do
    mutexp <- p_HdaudioSoftc_sc_stream_mtx sc
    mutexEnter mutexp
    scStreamMask <- peek =<< p_HdaudioSoftc_sc_stream_mask sc
    while 0 (intsts .&. scStreamMask)
    mutexExit mutexp
    hdaWrite4 sc e_HDAUDIO_MMIO_INTSTS e_HDAUDIO_INTSTS_SIS_MASK
  return 1
  where
    while :: Int -> Word32 -> IO ()
    while sid smask
      | sid >= e_HDAUDIO_MAX_STREAMS || smask == 0 = return ()
      | otherwise = do
        st <- p_HdaudioSoftc_sc_stream sc sid
        stIntr <- peek =<< p_HdaudioStream_st_intr st
        when (smask .&. 1 /= 0 && stIntr /= nullFunPtr) $
          call_HdaudioStream_st_intr stIntr st >> return ()
        while (sid + 1) (smask `shiftR` 1)
