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

foreign export ccall "hdaudioIntr" hdaudioIntr :: Ptr HdaudioSoftc -> IO Int
hdaudioIntr :: Ptr HdaudioSoftc -> IO Int
hdaudioIntr sc = do
  intsts <- hdaRead4 sc e_HDAUDIO_MMIO_INTSTS
  if intsts .&. e_HDAUDIO_INTSTS_GIS /= 0 then hdaudioIntr' sc intsts
    else return 0

hdaudioIntr' :: Ptr HdaudioSoftc -> Word32 -> IO Int
hdaudioIntr' sc intsts = do
  when (intsts .&. e_HDAUDIO_INTSTS_CIS /= 0) $ do
    rirbsts <- hdaRead1 sc e_HDAUDIO_MMIO_RIRBSTS
    when (rirbsts .&. e_HDAUDIO_RIRBSTS_RINTFL /= 0) $ do
      mutexp <- p_HdaudioSoftc_sc_corb_mtx sc
      mutexEnter mutexp
      c_hdaudio_rirb_dequeue sc True
      mutexExit mutexp
    when (rirbsts .&. (e_HDAUDIO_RIRBSTS_RIRBOIS .|. e_HDAUDIO_RIRBSTS_RINTFL) /= 0) $
      hdaWrite1 sc e_HDAUDIO_MMIO_RIRBSTS rirbsts
    hdaWrite4 sc e_HDAUDIO_MMIO_INTSTS e_HDAUDIO_INTSTS_CIS
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

foreign import ccall "hs_extern.h hdaudio_rirb_dequeue"
  c_hdaudio_rirb_dequeue :: Ptr HdaudioSoftc -> Bool -> IO Word32
