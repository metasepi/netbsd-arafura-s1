{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Hdaudio.Hdaudio () where
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.C.Types
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
    hdaudioRirbUnsol sc entry
    hdaudioRirbDequeue sc unsol
  else return $ rirbEntry_resp entry

hdaudioRirbUnsol :: Ptr HdaudioSoftc -> RirbEntry -> IO ()
hdaudioRirbUnsol sc entry = do
  let codecid = f_RIRB_CODEC_ID entry
  if codecid >= e_HDAUDIO_MAX_CODECS then
    hdaErrorI1 sc "unsol: codec id 0x%02x out of range\n" (fromIntegral codecid)
  else do
    co <- p_HdaudioSoftc_sc_codec sc (fromIntegral codecid)
    coValid <- peek =<< p_HdaudioCodec_co_valid co
    if not coValid then
      hdaErrorI1 sc "unsol: codec id 0x%02x not valid\n" (fromIntegral codecid)
    else p_HdaudioCodec_co_nfg co >>= peek >>= while co 0
  where
    while :: Ptr HdaudioCodec -> CUInt -> CUInt -> IO ()
    while co i coNfg | i < coNfg = do
                       fgh <- peek =<< p_HdaudioCodec_co_fg co
                       let fg = fgh `plusPtr` (sizeOf_HdaudioFunctionGroup * fromIntegral i)
                       fgDevice <- peek =<< p_HdaudioFunctionGroup_fg_device fg
                       fgUnsol <- peek =<< p_HdaudioFunctionGroup_fg_unsol fg
                       when (fgDevice /= nullPtr && fgUnsol /= nullFunPtr) $
                         void $ call_HdaudioFunctionGroup_fg_unsol fgUnsol fgDevice (fromIntegral $ rirbEntry_resp entry)
                     | otherwise = return ()
