{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudio () where
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Sys.Bus
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
      hdaudioRirbDequeue sc True
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

foreign export ccall "hdaudioRirbDequeue" hdaudioRirbDequeue :: Ptr HdaudioSoftc -> Bool -> IO Word32
hdaudioRirbDequeue :: Ptr HdaudioSoftc -> Bool -> IO Word32
hdaudioRirbDequeue sc unsol = do
  scRirbrp <- peek =<< p_HdaudioSoftc_sc_rirbrp sc
  let while :: Int -> Word16 -> IO (Either Word32 Int)
      while retry rirbwp
        | retry > 0 && rirbwp .&. 0xff == scRirbrp =
          if unsol then return $ Left 0xffffffff
          else do
            hdaDelay 10
            rirbwp' <- hdaRead2 sc e_HDAUDIO_MMIO_RIRBWP
            while (retry - 1) rirbwp'
        | otherwise = return $ Right retry
  r <- while (e_HDAUDIO_RIRB_TIMEOUT - 1) =<< hdaRead2 sc e_HDAUDIO_MMIO_RIRBWP
  case r of
    Left e -> return e
    Right retry -> if retry == 0 then hdaError0 sc "RIRB timeout\n" >> return 0xffffffff
                   else hdaudioRirbDequeue' sc unsol

hdaudioRirbDequeue' :: Ptr HdaudioSoftc -> Bool -> IO Word32
hdaudioRirbDequeue' sc unsol = do
  rirb <- f_DMA_KERNADDR =<< p_HdaudioSoftc_sc_rirb sc
  scRirbrp_p <- p_HdaudioSoftc_sc_rirbrp sc
  dmaSize <- peek =<< p_HdaudioDma_dma_size =<< p_HdaudioSoftc_sc_rirb sc
  let update :: Word16 -> IO Word16
      update scRirbrp = do
        let scRirbrp' = scRirbrp + 1
        if fromIntegral scRirbrp' >= (dmaSize `div` (fromIntegral sizeOf_RirbEntry)) then return 0
          else return scRirbrp'
  poke scRirbrp_p =<< update =<< peek scRirbrp_p
  dmat <- peek =<< p_HdaudioSoftc_sc_dmat sc
  dmaMap <- peek =<< p_HdaudioDma_dma_map =<< p_HdaudioSoftc_sc_rirb sc
  busDmamapSync dmat dmaMap 0 dmaSize e_BUS_DMASYNC_POSTREAD
  scRirbrp <- peek scRirbrp_p
  entry <- peekByteOff (castPtr rirb) (sizeOf_RirbEntry * fromIntegral scRirbrp)
  busDmamapSync dmat dmaMap 0 dmaSize e_BUS_DMASYNC_PREREAD
  if f_RIRB_UNSOL entry then do
    let goUnsol p = poke p entry >> c_hdaudio_rirb_unsol sc p
    alloca goUnsol
    hdaudioRirbDequeue sc unsol
  else return $ rirbEntry_resp entry

foreign import ccall "hs_extern.h hdaudio_rirb_unsol"
  c_hdaudio_rirb_unsol :: Ptr HdaudioSoftc -> Ptr RirbEntry -> IO ()
