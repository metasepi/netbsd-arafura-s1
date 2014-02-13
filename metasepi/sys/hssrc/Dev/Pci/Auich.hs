{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auich () where
import Control.Monad
import Data.Bits
import Data.List
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Kern.KernMutex
import Kern.SubrPrf
import Arch.I386.Include.Cpu
import Arch.I386.Include.Types
import Sys.Types
import Sys.Errno
import Sys.Audioio
import Sys.Bus
import Sys.Device
import Kern.SubrKmem
import Lib.Libkern.Libkern
import Dev.Pci.Auichreg
import Dev.Ic.Ac97reg
import Dev.Ic.Ac97var
import Dev.AudioIf
import Dev.Auconv
import Metasepi.EitherIO

foreign export ccall "auichOpen" auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen :: Ptr AuichSoftc -> Int -> IO Int
auichOpen sc flags = do
  mutexp <- p_AuichSoftc_sc_intr_lock sc
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  lock <- peek =<< p_Ac97CodecIfVtbl_lock =<< peek =<< p_Ac97CodecIf_vtbl codecif
  mutexSpinExit mutexp
  call_Ac97CodecIfVtbl_lock lock codecif
  mutexSpinEnter mutexp
  return 0

foreign export ccall "auichClose" auichClose :: Ptr AuichSoftc -> IO ()
auichClose :: Ptr AuichSoftc -> IO ()
auichClose sc = do
  mutexp <- p_AuichSoftc_sc_intr_lock sc
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  unlock <- peek =<< p_Ac97CodecIfVtbl_unlock =<< peek =<< p_Ac97CodecIf_vtbl codecif
  mutexSpinExit mutexp
  call_Ac97CodecIfVtbl_unlock unlock codecif
  mutexSpinEnter mutexp

foreign export ccall "auichQueryEncoding"
  auichQueryEncoding :: Ptr AuichSoftc -> Ptr AudioEncodingT -> IO Int
auichQueryEncoding :: Ptr AuichSoftc -> Ptr AudioEncodingT -> IO Int
auichQueryEncoding sc aep = do
  spdif <- peek =<< p_AuichSoftc_sc_spdif sc
  let p_ofs = if spdif then p_AuichSoftc_sc_spdif_encodings else p_AuichSoftc_sc_encodings
  encodings <- peek =<< p_ofs sc
  return =<< auconvQueryEncoding encodings aep

foreign export ccall "auichSetParams"
  auichSetParams :: Ptr AuichSoftc -> Int -> Int -> Ptr AudioParamsT -> Ptr AudioParamsT -> Ptr StreamFilterList -> Ptr StreamFilterList -> IO Int
auichSetParams :: Ptr AuichSoftc -> Int -> Int -> Ptr AudioParamsT -> Ptr AudioParamsT -> Ptr StreamFilterList -> Ptr StreamFilterList -> IO Int
auichSetParams sc setmode usemode play record pfil rfil = do
  let f = auichSetParams' sc setmode usemode play record pfil rfil
  r <- f e_AudioInfoT_mode_AUMODE_RECORD >>? f e_AudioInfoT_mode_AUMODE_PLAY
  either return (const $ return 0) r

auichSetParams' :: Ptr AuichSoftc -> Int -> Int -> Ptr AudioParamsT -> Ptr AudioParamsT -> Ptr StreamFilterList -> Ptr StreamFilterList -> Int -> IO (Either Int ())
auichSetParams' sc setmode usemode play record pfil rfil mode = do
  if setmode .&. mode == 0 then return $ Right () -- continue
  else do
    let param = if mode == e_AudioInfoT_mode_AUMODE_PLAY then play else record
        fil   = if mode == e_AudioInfoT_mode_AUMODE_PLAY then pfil else rfil
    if param == nullPtr then return $ Right () -- continue
    else do
      codectype <- peek =<< p_AuichSoftc_sc_codectype sc
      f6 codectype (param, fil) >>=? f7 >>=? f8 codectype >>=? f9 codectype
  where
    f6 :: Int -> (Ptr AudioParamsT, Ptr StreamFilterList) -> IO (Either Int (Ptr AudioParamsT, Ptr StreamFilterList, Int))
    f6 codectype (param, fil) = do
      rate <- peek =<< p_AudioParams_sample_rate param
      if (codectype == e_AC97_CODEC_TYPE_AUDIO) then
        if rate <  8000 || rate > 48000 then return $ Left e_EINVAL
        else do
          spdif <- peek =<< p_AuichSoftc_sc_spdif sc
          if spdif then do
            fmt <- c_get_auich_spdif_formats
            index <- auconvSetConverter fmt e_AUICH_SPDIF_NFORMATS mode param bTRUE fil
            return $ Right (param, fil, index)
          else do
            fmt <- p_AuichSoftc_sc_audio_formats sc 0
            index <- auconvSetConverter fmt e_AUICH_AUDIO_NFORMATS mode param bTRUE fil
            return $ Right (param, fil, index)
      else do
        if rate /=  8000 || rate /= 16000 then return $ Left e_EINVAL
        else do
          fmt <- p_AuichSoftc_sc_modem_formats sc 0
          index <- auconvSetConverter fmt e_AUICH_MODEM_NFORMATS mode param bTRUE fil
          return $ Right (param, fil, index)
    f7 :: (Ptr AudioParamsT, Ptr StreamFilterList, Int) -> IO (Either Int (Ptr AudioParamsT, Ptr StreamFilterList, Int))
    f7 (param, fil, index) =
      if index < 0 then return $ Left e_EINVAL
      else do
        s <- peek =<< p_StreamFilterList_req_size fil
        if s > 0 then do
          p' <- p_StreamFilterReq_param =<< p_StreamFilterList_filters fil 0
          return $ Right (p', fil, index)
        else return $ Right (param, fil, index)
    f8 :: Int -> (Ptr AudioParamsT, Ptr StreamFilterList, Int) -> IO (Either Int (Ptr AudioParamsT, Ptr StreamFilterList, Int))
    f8 codectype (param, fil, index) = do
      if (codectype == e_AC97_CODEC_TYPE_AUDIO) then do
        ft <- peek =<< p_AudioFormat_frequency_type =<< p_AuichSoftc_sc_audio_formats sc index
        if ft /= 1 then do
          rate <- peek =<< p_AudioParams_sample_rate param
          r <- auichSetRate sc mode $ fromIntegral rate
          if r /= 0 then return $ Left e_EINVAL else return $ Right (param, fil, index)
        else return $ Right (param, fil, index)
      else do
        ft <- peek =<< p_AudioFormat_frequency_type =<< p_AuichSoftc_sc_modem_formats sc index
        if ft /= 1 then do
          rate <- peek =<< p_AudioParams_sample_rate param
          r <- auichSetRate sc mode $ fromIntegral rate
          if r /= 0 then return $ Left e_EINVAL
          else do
            auichWriteCodec sc e_AC97_REG_LINE1_RATE $ fromIntegral rate
            auichWriteCodec sc e_AC97_REG_LINE1_LEVEL 0
            return $ Right (param, fil, index)
        else return $ Right (param, fil, index)
    f9 :: Int -> (Ptr AudioParamsT, Ptr StreamFilterList, Int) -> IO (Either Int ())
    f9 codectype (param, fil, index) = do
      when (mode == e_AudioInfoT_mode_AUMODE_PLAY && codectype == e_AC97_CODEC_TYPE_AUDIO) $ do
        iot <- peek =<< p_AuichSoftc_iot sc
        aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
        modem_offset <- peek =<< p_AuichSoftc_sc_modem_offset sc
        control <- busSpaceRead4 iot aud_ioh (e_ICH_GCTRL + modem_offset)
        channels <- peek =<< p_AudioParamsT_channels param
        scPcm246Mask <- peek =<< p_AuichSoftc_sc_pcm246_mask sc
        scPcm4 <- peek =<< p_AuichSoftc_sc_pcm4 sc
        scPcm6 <- peek =<< p_AuichSoftc_sc_pcm6 sc
        let control' = control .&. (complement scPcm246Mask) .|. (if channels == 4 then scPcm4 else scPcm6)
        busSpaceWrite4 iot aud_ioh (e_ICH_GCTRL + modem_offset) control'
      return $ Right ()

auichSetRate :: Ptr AuichSoftc -> Int -> CULong -> IO Int
auichSetRate sc mode srate = do
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  sc97Clock <- peek =<< p_AuichSoftc_sc_ac97_clock sc
  setClock <- peek =<< p_Ac97CodecIfVtbl_set_clock =<< peek =<< p_Ac97CodecIf_vtbl codecif
  call_Ac97CodecIfVtbl_set_clock setClock codecif (fromIntegral sc97Clock)
  setRatePtr <- peek =<< p_Ac97CodecIfVtbl_set_rate =<< peek =<< p_Ac97CodecIf_vtbl codecif
  let setRate = call_Ac97CodecIfVtbl_set_rate setRatePtr
  if e_AudioInfoT_mode_AUMODE_RECORD == mode then
    alloca $ \p -> poke p (fromIntegral srate) >> setRate codecif e_AC97_REG_PCM_LR_ADC_RATE p
  else do
    r <- alloca $ \p -> poke p (fromIntegral srate) >> setRate codecif e_AC97_REG_PCM_FRONT_DAC_RATE p
    if r /= 0 then return r
    else do
      r <- alloca $ \p -> poke p (fromIntegral srate) >> setRate codecif e_AC97_REG_PCM_SURR_DAC_RATE p
      if r /= 0 then return r
      else
        alloca $ \p -> poke p (fromIntegral srate) >> setRate codecif e_AC97_REG_PCM_LFE_DAC_RATE p

foreign export ccall "auichWriteCodec"
  auichWriteCodec :: Ptr AuichSoftc -> Word8 -> Word16 -> IO Int
auichWriteCodec :: Ptr AuichSoftc -> Word8 -> Word16 -> IO Int
auichWriteCodec sc reg val = do
  iot <- peek =<< p_AuichSoftc_iot sc
  audIoh <- peek =<< p_AuichSoftc_aud_ioh sc
  modemOffset <- peek =<< p_AuichSoftc_sc_modem_offset sc
  let waitSemaphore :: IO (Either () ())
      waitSemaphore = do
        r <- busSpaceRead1 iot audIoh (e_ICH_CAS + modemOffset)
        if (r .&. 1 /= 0) then return $ Left ()
        else do delay e_ICH_CODECIO_INTERVAL
                return $ Right ()
      waitSemaphore10 = foldl' (>>?) (return $ Right ()) (replicate 10 waitSemaphore)
      t = fromIntegral $ e_ICH_SEMATIMO `div` e_ICH_CODECIO_INTERVAL `div` 10
  r <- foldl' (>>?) (return $ Right ()) (replicate t waitSemaphore10)
  case r of
    Left () -> do
      mixIoh <- peek =<< p_AuichSoftc_mix_ioh sc
      codecnum <- peek =<< p_AuichSoftc_sc_codecnum sc
      busSpaceWrite2 iot mixIoh (fromIntegral codecnum * e_ICH_CODEC_OFFSET + fromIntegral reg) val
      return 0
    Right () -> do
      dev <- peek =<< p_AuichSoftc_sc_dev sc
      aprintNormalDev0 dev "write_codec timeout\n"
      return (-1)

foreign export ccall "auichRoundBlocksize"
  auichRoundBlocksize :: Ptr AuichSoftc -> Int -> Int -> Ptr AudioParamsT -> IO Int
auichRoundBlocksize :: Ptr AuichSoftc -> Int -> Int -> Ptr AudioParamsT -> IO Int
auichRoundBlocksize sc blk mode param = do
  return $ blk .&. complement 0x3f

foreign export ccall "auichHaltPipe"
  auichHaltPipe :: Ptr AuichSoftc -> Int -> IO ()
auichHaltPipe :: Ptr AuichSoftc -> Int -> IO ()
auichHaltPipe sc pipe = do
  iot <- peek =<< p_AuichSoftc_iot sc
  audIoh <- peek =<< p_AuichSoftc_aud_ioh sc
  busSpaceWrite1 iot audIoh (fromIntegral pipe + e_ICH_CTRL) 0
  let f :: IO (Either () ())
      f = do
        s <- busSpaceRead4 iot audIoh $ fromIntegral pipe + e_ICH_STS
        if s .&. e_ICH_DCH /= 0 then return $ Left ()
        else delay 1 >> (return $ Right ())
      f10 = foldl' (>>?) (return $ Right ()) (replicate 10 f)
  foldl' (>>?) (return $ Right ()) (replicate 10 f10)
  busSpaceWrite1 iot audIoh (fromIntegral pipe + e_ICH_CTRL) e_ICH_RR

foreign export ccall "auichHaltOutput"
  auichHaltOutput :: Ptr AuichSoftc -> IO Int
auichHaltOutput :: Ptr AuichSoftc -> IO Int
auichHaltOutput sc = do
  auichHaltPipe sc e_ICH_PCMO
  intr <- p_AuichRing_intr =<< p_AuichSoftc_pcmo sc
  poke (castPtr intr) (0::IntPtr)
  return 0

foreign export ccall "auichHaltInput"
  auichHaltInput :: Ptr AuichSoftc -> IO Int
auichHaltInput :: Ptr AuichSoftc -> IO Int
auichHaltInput sc = do
  auichHaltPipe sc e_ICH_PCMI
  intr <- p_AuichRing_intr =<< p_AuichSoftc_pcmi sc
  poke (castPtr intr) (0::IntPtr)
  return 0

foreign export ccall "auichGetdev"
  auichGetdev :: Ptr AuichSoftc -> Ptr AudioDevice -> IO Int
auichGetdev :: Ptr AuichSoftc -> Ptr AudioDevice -> IO Int
auichGetdev sc adp = do
  audev <- p_AuichSoftc_sc_audev sc
  memcpy adp audev $ fromIntegral sizeOf_AudioDevice
  return 0

foreign export ccall "auichSetPort"
  auichSetPort :: Ptr AuichSoftc -> Ptr MixerCtrl -> IO Int
auichSetPort :: Ptr AuichSoftc -> Ptr MixerCtrl -> IO Int
auichSetPort sc cp = do
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  f <- peek =<< p_Ac97CodecIfVtbl_mixer_set_port =<< peek =<< p_Ac97CodecIf_vtbl codecif
  call_Ac97CodecIfVtbl_mixer_set_port f codecif cp

foreign export ccall "auichGetPort"
  auichGetPort :: Ptr AuichSoftc -> Ptr MixerCtrl -> IO Int
auichGetPort :: Ptr AuichSoftc -> Ptr MixerCtrl -> IO Int
auichGetPort sc cp = do
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  f <- peek =<< p_Ac97CodecIfVtbl_mixer_get_port =<< peek =<< p_Ac97CodecIf_vtbl codecif
  call_Ac97CodecIfVtbl_mixer_get_port f codecif cp

foreign export ccall "auichQueryDevinfo"
  auichQueryDevinfo :: Ptr AuichSoftc -> Ptr MixerDevinfo -> IO Int
auichQueryDevinfo :: Ptr AuichSoftc -> Ptr MixerDevinfo -> IO Int
auichQueryDevinfo sc dp = do
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  f <- peek =<< p_Ac97CodecIfVtbl_query_devinfo =<< peek =<< p_Ac97CodecIf_vtbl codecif
  call_Ac97CodecIfVtbl_query_devinfo f codecif dp

foreign export ccall "auichAllocmem"
  auichAllocmem :: Ptr AuichSoftc -> CSize -> CSize -> Ptr AuichDma -> IO Int
auichAllocmem :: Ptr AuichSoftc -> CSize -> CSize -> Ptr AuichDma -> IO Int
auichAllocmem sc size align p = do
  pS <- p_AuichDma_size p
  poke pS size
  dmat <- peek =<< p_AuichSoftc_dmat sc
  map_p <- p_AuichDma_map p
  addr_p <- p_AuichDma_addr p
  size <- peek =<< p_AuichDma_size p
  segs <- p_AuichDma_segs p 0
  nsegs_p <- p_AuichDma_nsegs p
  flags <- peek =<< p_AuichSoftc_sc_dmamap_flags sc
  let cleanupDestroy = peek map_p >>= busDmamapDestroy dmat
      cleanupUnmap   = peek addr_p >>= flip (busDmamemUnmap dmat) size
      cleanupFree    = peek nsegs_p >>= busDmamemFree dmat segs
      fAlloc, fMap, fCreate, fLoad :: [IO ()] -> IO (Either (Int, [IO ()]) [IO ()])
      fAlloc as = do
        r <- busDmamemAlloc dmat size align 0 segs e_AUICH_NUM_SEGS nsegs_p e_BUS_DMA_WAITOK
        if r /= 0 then return $ Left (r, as)
        else return $ Right $ cleanupFree:as
      fMap as = do
        nsegs <- peek nsegs_p
        r <- busDmamemMap dmat segs nsegs size addr_p $ e_BUS_DMA_WAITOK .|. flags
        if r /= 0 then return $ Left (r, as)
        else return $ Right $ cleanupUnmap:as
      fCreate as = do
        r <- busDmamapCreate dmat size 1 size 0 e_BUS_DMA_WAITOK map_p
        if r /= 0 then return $ Left (r, as)
        else return $ Right $ cleanupDestroy:as
      fLoad as = do
        map <- peek map_p
        addr <- peek addr_p
        r <- busDmamapLoad dmat map addr size nullPtr e_BUS_DMA_WAITOK
        if r /= 0 then return $ Left (r, as)
        else return $ Right []
  r <- fAlloc [] >>=? fMap >>=? fCreate >>=? fLoad
  case r of
    Right _ -> return 0
    Left (e, as) -> sequence_ as >> return e

foreign export ccall "auichFreemem"
  auichFreemem :: Ptr AuichSoftc -> Ptr AuichDma -> IO Int
auichFreemem :: Ptr AuichSoftc -> Ptr AuichDma -> IO Int
auichFreemem sc p = do
  dmat <- peek =<< p_AuichSoftc_dmat sc
  map <- peek =<< p_AuichDma_map p
  addr <- peek =<< p_AuichDma_addr p
  size <- peek =<< p_AuichDma_size p
  segs <- p_AuichDma_segs p 0
  nsegs <- peek =<< p_AuichDma_nsegs p
  busDmamapUnload dmat map
  busDmamapDestroy dmat map
  busDmamemUnmap dmat addr size
  busDmamemFree dmat segs nsegs
  return 0

foreign export ccall "auichAllocm"
  auichAllocm :: Ptr AuichSoftc -> Int -> CSize -> IO (Ptr ())
auichAllocm :: Ptr AuichSoftc -> Int -> CSize -> IO (Ptr ())
auichAllocm sc direction size =
  if size > e_ICH_DMALIST_MAX * e_ICH_DMASEG_MAX then return nullPtr
  else do
    p <- fmap castPtr $ kmemAlloc (fromIntegral sizeOf_AuichDma) e_KM_SLEEP
    if p == nullPtr then return nullPtr
    else do
      error <- auichAllocmem sc size 0 p
      if error /= 0 then kmemFree (castPtr p) (fromIntegral sizeOf_AuichDma) >> return nullPtr
      else do
        next_p <- p_AuichDma_next p
        dmas_p <- p_AuichSoftc_sc_dmas sc
        dmas   <- peek dmas_p
        poke next_p dmas
        poke dmas_p p
        kernAddr_AuichDma p

foreign export ccall "auichFreem"
  auichFreem :: Ptr AuichSoftc -> Ptr () -> CSize -> IO ()
auichFreem :: Ptr AuichSoftc -> Ptr () -> CSize -> IO ()
auichFreem sc ptr size =
  let while :: Ptr (Ptr AuichDma) -> IO ()
      while pp = do
        p <- peek pp
        if p == nullPtr then return ()
        else do
          a <- kernAddr_AuichDma p
          if a == ptr then do
            next <- peek =<< p_AuichDma_next p
            poke pp next
            kmemFree (castPtr p) (fromIntegral sizeOf_AuichDma)
            return ()
          else p_AuichDma_next p >>= while
  in p_AuichSoftc_sc_dmas sc >>= while

foreign export ccall "auichRoundBuffersize"
  auichRoundBuffersize :: Ptr AuichSoftc -> Int -> CSize -> IO CSize
auichRoundBuffersize :: Ptr AuichSoftc -> Int -> CSize -> IO CSize
auichRoundBuffersize sc direction size =
  let m = fromIntegral e_ICH_DMALIST_MAX * fromIntegral e_ICH_DMASEG_MAX
  in return $ if size > m then m else size

foreign export ccall "auichMappage"
  auichMappage :: Ptr AuichSoftc -> Ptr () -> OffT -> Int -> IO PaddrT
auichMappage :: Ptr AuichSoftc -> Ptr () -> OffT -> Int -> IO PaddrT
auichMappage sc mem off prot =
  if off < 0 then return (-1)
  else
    p_AuichSoftc_sc_dmas sc >>= peek >>= while
  where
    while :: Ptr AuichDma -> IO PaddrT
    while p =
      if p /= nullPtr then do
        a <- kernAddr_AuichDma p
        if a /= mem then p_AuichDma_next p >>= peek >>= while
        else go p
      else go p
    go :: Ptr AuichDma -> IO PaddrT
    go p =
      if p == nullPtr then return (-1)
      else do
        dmat <- peek =<< p_AuichSoftc_dmat sc
        segs <- p_AuichDma_segs p 0
        nsegs <- peek =<< p_AuichDma_nsegs p
        busDmamemMmap dmat segs nsegs off prot e_BUS_DMA_WAITOK

foreign export ccall "auichGetProps"
  auichGetProps :: Ptr AuichSoftc -> IO Int
auichGetProps :: Ptr AuichSoftc -> IO Int
auichGetProps sc = do
  let props = e_AUDIO_PROP_INDEPENDENT .|. e_AUDIO_PROP_FULLDUPLEX
  codecif <- peek =<< p_AuichSoftc_codec_if sc
  codectype <- peek =<< p_AuichSoftc_sc_codectype sc
  fixed <- f_AC97_IS_FIXED_RATE codecif
  return $ if not fixed || codectype == e_AC97_CODEC_TYPE_MODEM then props .|. e_AUDIO_PROP_MMAP
           else props

foreign export ccall "auichGetLocks"
  auichGetLocks :: Ptr AuichSoftc -> Ptr (Ptr KmutexT) -> Ptr (Ptr KmutexT) -> IO ()
auichGetLocks :: Ptr AuichSoftc -> Ptr (Ptr KmutexT) -> Ptr (Ptr KmutexT) -> IO ()
auichGetLocks sc intr thread = do
  i <- p_AuichSoftc_sc_intr_lock sc
  t <- p_AuichSoftc_sc_lock sc
  poke intr i
  poke thread t

foreign export ccall "auichTriggerPipe"
  auichTriggerPipe :: Ptr AuichSoftc -> Int -> Ptr AuichRing -> IO ()
auichTriggerPipe :: Ptr AuichSoftc -> Int -> Ptr AuichRing -> IO ()
auichTriggerPipe sc pipe ring = do
  blksize <- fmap fromIntegral $ peek =<< p_AuichRing_blksize ring
  dmalist <- peek =<< p_AuichRing_dmalist ring
  sampleShift <- peek =<< p_AuichSoftc_sc_sample_shift sc
  let while :: Int -> IO ()
      while qptr | qptr < fromIntegral e_ICH_DMALIST_MAX = next qptr
                 | otherwise = fin qptr
      next qptr = do
        let q = dmalist `plusPtr` (sizeOf_AuichDmalist * qptr)
        ringP_p <- p_AuichRing_p ring
        qBase_p <- p_AuichDmalist_base q
        qLen_p <- p_AuichDmalist_len q
        poke qBase_p =<< peek ringP_p
        poke qLen_p $ shiftR blksize sampleShift .|. e_ICH_DMAF_IOC
        v <- fmap (+ blksize) $ peek ringP_p
        poke ringP_p v
        end <- peek =<< p_AuichRing_end ring
        when (v >= end) $ poke ringP_p =<< peek =<< p_AuichRing_start ring
        while $ qptr + 1
      fin qptr = do
        q <- p_AuichRing_qptr ring
        poke q 0
        iot <- peek =<< p_AuichSoftc_iot sc
        aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
        busSpaceWrite1 iot aud_ioh (fromIntegral pipe + e_ICH_LVI) $ fromIntegral (qptr - 1) .&. e_ICH_LVI_MASK
        busSpaceWrite1 iot aud_ioh (fromIntegral pipe + e_ICH_CTRL) $ e_ICH_IOCE .|. e_ICH_FEIE .|. e_ICH_RPBM
  while 0

foreign export ccall "auichTriggerOutput"
  auichTriggerOutput :: Ptr AuichSoftc -> Ptr () -> Ptr () -> Int -> (FunPtr ((Ptr ()) -> IO ())) -> Ptr () -> Ptr AudioParamsT -> IO Int
auichTriggerOutput :: Ptr AuichSoftc -> Ptr () -> Ptr () -> Int -> (FunPtr ((Ptr ()) -> IO ())) -> Ptr () -> Ptr AudioParamsT -> IO Int
auichTriggerOutput sc start end blksize intr arg param =
  p_AuichSoftc_sc_dmas sc >>= peek >>= while
  where
    while :: Ptr AuichDma -> IO Int
    while p | p /= nullPtr = do a <- kernAddr_AuichDma p
                                if a /= start then p_AuichDma_next p >>= peek >>= while
                                else go p
            | otherwise = go p
    go :: Ptr AuichDma -> IO Int
    go p | p == nullPtr = printfP1 "auich_trigger_output: bad addr %p\n" start >> return e_EINVAL
    go p = do
      let size = fromIntegral $ end `minusPtr` start
      pcmo <- p_AuichSoftc_pcmo sc
      (flip poke) intr =<< p_AuichRing_intr pcmo
      (flip poke) arg =<< p_AuichRing_arg pcmo
      daddr <- fmap fromIntegral $ dmaAddr p
      (flip poke) daddr =<< p_AuichRing_start pcmo
      (flip poke) daddr =<< p_AuichRing_p pcmo
      (flip poke) (daddr + size) =<< p_AuichRing_end pcmo
      (flip poke) blksize =<< p_AuichRing_blksize pcmo
      iot <- peek =<< p_AuichSoftc_iot sc
      aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
      cddma <- dmaMapAddr =<< peek =<< p_AuichSoftc_sc_cddmamap sc
      busSpaceWrite4 iot aud_ioh (fromIntegral (e_ICH_PCMO + e_ICH_BDBAR)) $
        fromIntegral (cddma + fromIntegral (f_ICH_PCMO_OFF 0))
      auichTriggerPipe sc e_ICH_PCMO pcmo
      return 0

-- !!! INTR !!!
foreign export ccall "auichIntrPipe"
  auichIntrPipe :: Ptr AuichSoftc -> Int -> Ptr AuichRing -> IO ()
auichIntrPipe :: Ptr AuichSoftc -> Int -> Ptr AuichRing -> IO ()
auichIntrPipe sc pipe ring = do
  blksize <- fmap fromIntegral $ peek =<< p_AuichRing_blksize ring
  iot <- peek =<< p_AuichSoftc_iot sc
  aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
  nqptr <- busSpaceRead1 iot aud_ioh $ fromIntegral pipe + e_ICH_CIV
  sampleShift <- peek =<< p_AuichSoftc_sc_sample_shift sc
  ringP_p <- p_AuichRing_p ring
  ringStart <- peek =<< p_AuichRing_start ring
  ringEnd <- peek =<< p_AuichRing_end ring
  ringIntr <- peek =<< p_AuichRing_intr ring
  let while ::Int -> IO ()
      while qptr | fromIntegral qptr == nqptr = post qptr
                 | otherwise = do
                   dmalist <- peek =<< p_AuichRing_dmalist ring
                   let q = dmalist `plusPtr` (sizeOf_AuichDmalist * qptr)
                   qBase_p <- p_AuichDmalist_base q
                   p <- peek ringP_p
                   poke qBase_p p
                   qLen_p <- p_AuichDmalist_len q
                   poke qLen_p $ shiftR blksize sampleShift .|. e_ICH_DMAF_IOC
                   let p' = p + blksize
                   poke ringP_p $ if p' >= ringEnd then ringStart else p'
                   when (ringIntr /= nullFunPtr) $
                     p_AuichRing_arg ring >>= peek >>= call_AuichRing_intr ringIntr
                   while $ (qptr + 1) .&. fromIntegral e_ICH_LVI_MASK
      post :: Int -> IO ()
      post qptr = do
        (flip poke) qptr =<< p_AuichRing_qptr ring
        iot <- peek =<< p_AuichSoftc_iot sc
        aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
        busSpaceWrite1 iot aud_ioh (fromIntegral pipe + e_ICH_LVI) $
          (fromIntegral qptr - 1) .&. e_ICH_LVI_MASK
  while =<< peek =<< p_AuichRing_qptr ring

-- !!! INTR !!!
foreign export ccall "auichIntr"
  auichIntr :: Ptr AuichSoftc -> Int -> Int -> IO Int
auichIntr :: Ptr AuichSoftc -> Int -> Int -> IO Int
auichIntr sc gsts ret = do
  -- xxxxxxxxxxxxxx Not yet snatch all
  codectype <- peek =<< p_AuichSoftc_sc_codectype sc
  iot <- peek =<< p_AuichSoftc_iot sc
  aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
  modem_offset <- peek =<< p_AuichSoftc_sc_modem_offset sc
  let f3, post :: Int -> IO Int
      f3 r =
        if codectype == e_AC97_CODEC_TYPE_AUDIO && gsts .&. e_ICH_MINT /= 0 then do
          stsReg <- peek =<< p_AuichSoftc_sc_sts_reg sc
          sts <- busSpaceRead2 iot aud_ioh $ e_ICH_MICI + fromIntegral stsReg
          when (sts .&. e_ICH_FIFOE /= 0) $ do
            printfS1 "%s: fifo overrun\n" =<< deviceXname =<< peek =<< p_AuichSoftc_sc_dev sc
          when (sts .&. e_ICH_BCIS /= 0) $ do
            auichIntrPipe sc (fromIntegral e_ICH_MICI) =<< p_AuichSoftc_mici sc
          -- int ack
          busSpaceWrite2 iot aud_ioh (e_ICH_MICI + fromIntegral stsReg)
            (sts .&. (e_ICH_BCIS .|. e_ICH_FIFOE))
          busSpaceWrite4 iot aud_ioh (e_ICH_GSTS + modem_offset) $ fromIntegral e_ICH_MINT
          return $ r + 1
        else
          return r
      post r = do
        mutexp <- p_AuichSoftc_sc_intr_lock sc
        mutexSpinExit mutexp
        return r
  f3 ret >>= post

foreign import ccall "hs_extern.h get_auich_spdif_formats"
  c_get_auich_spdif_formats :: IO (Ptr AudioFormat)

foreign import primitive "const.AUICH_AUDIO_NFORMATS" e_AUICH_AUDIO_NFORMATS :: Int
foreign import primitive "const.AUICH_MODEM_NFORMATS" e_AUICH_MODEM_NFORMATS :: Int
foreign import primitive "const.AUICH_SPDIF_NFORMATS" e_AUICH_SPDIF_NFORMATS :: Int
foreign import primitive "const.ICH_CODECIO_INTERVAL" e_ICH_CODECIO_INTERVAL :: CUInt
foreign import primitive "const.AUICH_NUM_SEGS" e_AUICH_NUM_SEGS :: Int

--------------------------------------------------------------------------
-- Following code is generated by struct2hs command semi-automatically. --
--------------------------------------------------------------------------
newtype {-# CTYPE "struct auich_softc" #-} AuichSoftc = AuichSoftc ()
foreign import primitive "const.offsetof(struct auich_softc, sc_intr_lock)"
  offsetOf_AuichSoftc_sc_intr_lock :: Int
foreign import primitive "const.offsetof(struct auich_softc, codec_if)"
  offsetOf_AuichSoftc_codec_if :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_spdif)"
  offsetOf_AuichSoftc_sc_spdif :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_encodings)"
  offsetOf_AuichSoftc_sc_encodings :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_spdif_encodings)"
  offsetOf_AuichSoftc_sc_spdif_encodings :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_codectype)"
  offsetOf_AuichSoftc_sc_codectype :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_modem_offset)"
  offsetOf_AuichSoftc_sc_modem_offset :: Int
foreign import primitive "const.offsetof(struct auich_softc, iot)"
  offsetOf_AuichSoftc_iot :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_ioh)"
  offsetOf_AuichSoftc_aud_ioh :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm246_mask)"
  offsetOf_AuichSoftc_sc_pcm246_mask :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm4)"
  offsetOf_AuichSoftc_sc_pcm4 :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_pcm6)"
  offsetOf_AuichSoftc_sc_pcm6 :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_audio_formats)"
  offsetOf_AuichSoftc_sc_audio_formats :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_modem_formats)"
  offsetOf_AuichSoftc_sc_modem_formats :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_ac97_clock)"
  offsetOf_AuichSoftc_sc_ac97_clock :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_dev)"
  offsetOf_AuichSoftc_sc_dev :: Int
foreign import primitive "const.offsetof(struct auich_softc, mix_ioh)"
  offsetOf_AuichSoftc_mix_ioh :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_codecnum)"
  offsetOf_AuichSoftc_sc_codecnum :: Int
foreign import primitive "const.offsetof(struct auich_softc, pcmo)"
  offsetOf_AuichSoftc_pcmo :: Int
foreign import primitive "const.offsetof(struct auich_softc, pcmi)"
  offsetOf_AuichSoftc_pcmi :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_audev)"
  offsetOf_AuichSoftc_sc_audev :: Int
foreign import primitive "const.offsetof(struct auich_softc, dmat)"
  offsetOf_AuichSoftc_dmat :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_dmamap_flags)"
  offsetOf_AuichSoftc_sc_dmamap_flags :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_dmas)"
  offsetOf_AuichSoftc_sc_dmas :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_lock)"
  offsetOf_AuichSoftc_sc_lock :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_sample_shift)"
  offsetOf_AuichSoftc_sc_sample_shift :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_cddmamap)"
  offsetOf_AuichSoftc_sc_cddmamap :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_sts_reg)"
  offsetOf_AuichSoftc_sc_sts_reg :: Int
foreign import primitive "const.offsetof(struct auich_softc, mici)"
  offsetOf_AuichSoftc_mici :: Int

p_AuichSoftc_sc_intr_lock :: Ptr AuichSoftc -> IO (Ptr KmutexT)
p_AuichSoftc_sc_intr_lock p = return $ plusPtr p offsetOf_AuichSoftc_sc_intr_lock
p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
p_AuichSoftc_sc_spdif :: Ptr AuichSoftc -> IO (Ptr Bool)
p_AuichSoftc_sc_spdif p = return $ plusPtr p offsetOf_AuichSoftc_sc_spdif
p_AuichSoftc_sc_encodings :: Ptr AuichSoftc -> IO (Ptr (Ptr AudioEncodingSet))
p_AuichSoftc_sc_encodings p = return $ plusPtr p offsetOf_AuichSoftc_sc_encodings
p_AuichSoftc_sc_spdif_encodings :: Ptr AuichSoftc -> IO (Ptr (Ptr AudioEncodingSet))
p_AuichSoftc_sc_spdif_encodings p = return $ plusPtr p offsetOf_AuichSoftc_sc_spdif_encodings
p_AuichSoftc_sc_codectype :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_codectype p = return $ plusPtr p offsetOf_AuichSoftc_sc_codectype
p_AuichSoftc_sc_modem_offset :: Ptr AuichSoftc -> IO (Ptr BusSizeT)
p_AuichSoftc_sc_modem_offset p = return $ plusPtr p offsetOf_AuichSoftc_sc_modem_offset
p_AuichSoftc_iot :: Ptr AuichSoftc -> IO (Ptr BusSpaceTagT)
p_AuichSoftc_iot p = return $ plusPtr p offsetOf_AuichSoftc_iot
p_AuichSoftc_aud_ioh :: Ptr AuichSoftc -> IO (Ptr BusSpaceHandleT)
p_AuichSoftc_aud_ioh p = return $ plusPtr p offsetOf_AuichSoftc_aud_ioh
p_AuichSoftc_sc_pcm246_mask :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm246_mask p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm246_mask
p_AuichSoftc_sc_pcm4 :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm4 p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm4
p_AuichSoftc_sc_pcm6 :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_pcm6 p = return $ plusPtr p offsetOf_AuichSoftc_sc_pcm6
p_AuichSoftc_sc_audio_formats :: Ptr AuichSoftc -> Int -> IO (Ptr AudioFormat)
p_AuichSoftc_sc_audio_formats p i = return $ plusPtr p $ offsetOf_AuichSoftc_sc_audio_formats + i * sizeOf_AudioFormat
p_AuichSoftc_sc_modem_formats :: Ptr AuichSoftc -> Int -> IO (Ptr AudioFormat)
p_AuichSoftc_sc_modem_formats p i = return $ plusPtr p $ offsetOf_AuichSoftc_sc_modem_formats + i * sizeOf_AudioFormat
p_AuichSoftc_sc_ac97_clock :: Ptr AuichSoftc -> IO (Ptr Word32)
p_AuichSoftc_sc_ac97_clock p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_ac97_clock
p_AuichSoftc_sc_dev :: Ptr AuichSoftc -> IO (Ptr DeviceT)
p_AuichSoftc_sc_dev p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_dev
p_AuichSoftc_mix_ioh :: Ptr AuichSoftc -> IO (Ptr BusSpaceHandleT)
p_AuichSoftc_mix_ioh p = return $ plusPtr p $ offsetOf_AuichSoftc_mix_ioh
p_AuichSoftc_sc_codecnum :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_codecnum p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_codecnum
p_AuichSoftc_pcmo :: Ptr AuichSoftc -> IO (Ptr AuichRing)
p_AuichSoftc_pcmo p = return $ plusPtr p $ offsetOf_AuichSoftc_pcmo
p_AuichSoftc_pcmi :: Ptr AuichSoftc -> IO (Ptr AuichRing)
p_AuichSoftc_pcmi p = return $ plusPtr p $ offsetOf_AuichSoftc_pcmi
p_AuichSoftc_sc_audev :: Ptr AuichSoftc -> IO (Ptr AudioDevice)
p_AuichSoftc_sc_audev p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_audev
p_AuichSoftc_dmat :: Ptr AuichSoftc -> IO (Ptr BusDmaTagT)
p_AuichSoftc_dmat p = return $ plusPtr p $ offsetOf_AuichSoftc_dmat
p_AuichSoftc_sc_dmamap_flags :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_dmamap_flags p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_dmamap_flags
p_AuichSoftc_sc_dmas :: Ptr AuichSoftc -> IO (Ptr (Ptr AuichDma))
p_AuichSoftc_sc_dmas p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_dmas
p_AuichSoftc_sc_lock :: Ptr AuichSoftc -> IO (Ptr KmutexT)
p_AuichSoftc_sc_lock p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_lock
p_AuichSoftc_sc_sample_shift :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_sample_shift p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_sample_shift
p_AuichSoftc_sc_cddmamap :: Ptr AuichSoftc -> IO (Ptr BusDmamapT)
p_AuichSoftc_sc_cddmamap p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_cddmamap
p_AuichSoftc_sc_sts_reg :: Ptr AuichSoftc -> IO (Ptr Int)
p_AuichSoftc_sc_sts_reg p = return $ plusPtr p $ offsetOf_AuichSoftc_sc_sts_reg
p_AuichSoftc_mici :: Ptr AuichSoftc -> IO (Ptr AuichRing)
p_AuichSoftc_mici p = return $ plusPtr p $ offsetOf_AuichSoftc_mici

newtype {-# CTYPE "struct auich_ring" #-} AuichRing = AuichRing ()
foreign import primitive "const.sizeof(struct auich_ring)"
  sizeOf_AuichRing :: Int
foreign import primitive "const.offsetof(struct auich_ring, intr)"
  offsetOf_AuichRing_intr :: Int
p_AuichRing_intr :: Ptr AuichRing -> IO (Ptr (FunPtr ((Ptr ()) -> IO ())))
p_AuichRing_intr p = return $ plusPtr p $ offsetOf_AuichRing_intr
foreign import ccall "dynamic" call_AuichRing_intr ::
  FunPtr ((Ptr ()) -> IO ()) -> (Ptr ()) -> IO ()
foreign import primitive "const.offsetof(struct auich_ring, blksize)"
  offsetOf_AuichRing_blksize :: Int
p_AuichRing_blksize :: Ptr AuichRing -> IO (Ptr Int)
p_AuichRing_blksize p = return $ plusPtr p $ offsetOf_AuichRing_blksize
foreign import primitive "const.offsetof(struct auich_ring, dmalist)"
  offsetOf_AuichRing_dmalist :: Int
p_AuichRing_dmalist :: Ptr AuichRing -> IO (Ptr (Ptr AuichDmalist))
p_AuichRing_dmalist p = return $ plusPtr p $ offsetOf_AuichRing_dmalist
foreign import primitive "const.offsetof(struct auich_ring, p)"
  offsetOf_AuichRing_p :: Int
p_AuichRing_p :: Ptr AuichRing -> IO (Ptr Word32)
p_AuichRing_p p = return $ plusPtr p $ offsetOf_AuichRing_p
foreign import primitive "const.offsetof(struct auich_ring, start)"
  offsetOf_AuichRing_start :: Int
p_AuichRing_start :: Ptr AuichRing -> IO (Ptr Word32)
p_AuichRing_start p = return $ plusPtr p $ offsetOf_AuichRing_start
foreign import primitive "const.offsetof(struct auich_ring, end)"
  offsetOf_AuichRing_end :: Int
p_AuichRing_end :: Ptr AuichRing -> IO (Ptr Word32)
p_AuichRing_end p = return $ plusPtr p $ offsetOf_AuichRing_end
foreign import primitive "const.offsetof(struct auich_ring, qptr)"
  offsetOf_AuichRing_qptr :: Int
p_AuichRing_qptr :: Ptr AuichRing -> IO (Ptr Int)
p_AuichRing_qptr p = return $ plusPtr p $ offsetOf_AuichRing_qptr
foreign import primitive "const.offsetof(struct auich_ring, arg)"
  offsetOf_AuichRing_arg :: Int
p_AuichRing_arg :: Ptr AuichRing -> IO (Ptr (Ptr ()))
p_AuichRing_arg p = return $ plusPtr p $ offsetOf_AuichRing_arg

newtype {-# CTYPE "struct auich_dma" #-} AuichDma = AuichDma ()
foreign import primitive "const.sizeof(struct auich_dma)"
  sizeOf_AuichDma :: Int
foreign import primitive "const.offsetof(struct auich_dma, size)"
  offsetOf_AuichDma_size :: Int
p_AuichDma_size :: Ptr AuichDma -> IO (Ptr CSize)
p_AuichDma_size p = return $ plusPtr p $ offsetOf_AuichDma_size
foreign import primitive "const.offsetof(struct auich_dma, segs)"
  offsetOf_AuichDma_segs :: Int
p_AuichDma_segs :: Ptr AuichDma -> Int -> IO (Ptr BusDmaSegmentT)
p_AuichDma_segs p i = return $ plusPtr p $ offsetOf_AuichDma_segs + i * sizeOf_BusDmaSegmentT
foreign import primitive "const.offsetof(struct auich_dma, nsegs)"
  offsetOf_AuichDma_nsegs :: Int
p_AuichDma_nsegs :: Ptr AuichDma -> IO (Ptr Int)
p_AuichDma_nsegs p = return $ plusPtr p $ offsetOf_AuichDma_nsegs
foreign import primitive "const.offsetof(struct auich_dma, addr)"
  offsetOf_AuichDma_addr :: Int
p_AuichDma_addr :: Ptr AuichDma -> IO (Ptr (Ptr ()))
p_AuichDma_addr p = return $ plusPtr p $ offsetOf_AuichDma_addr
foreign import primitive "const.offsetof(struct auich_dma, map)"
  offsetOf_AuichDma_map :: Int
p_AuichDma_map :: Ptr AuichDma -> IO (Ptr BusDmamapT)
p_AuichDma_map p = return $ plusPtr p $ offsetOf_AuichDma_map
foreign import primitive "const.offsetof(struct auich_dma, next)"
  offsetOf_AuichDma_next :: Int
p_AuichDma_next :: Ptr AuichDma -> IO (Ptr (Ptr AuichDma))
p_AuichDma_next p = return $ plusPtr p $ offsetOf_AuichDma_next

newtype {-# CTYPE "struct auich_dmalist" #-} AuichDmalist = AuichDmalist ()
foreign import primitive "const.sizeof(struct auich_dmalist)"
  sizeOf_AuichDmalist :: Int
foreign import primitive "const.offsetof(struct auich_dmalist, base)"
  offsetOf_AuichDmalist_base :: Int
p_AuichDmalist_base :: Ptr AuichDmalist -> IO (Ptr Word32)
p_AuichDmalist_base p = return $ plusPtr p $ offsetOf_AuichDmalist_base
foreign import primitive "const.offsetof(struct auich_dmalist, len)"
  offsetOf_AuichDmalist_len :: Int
p_AuichDmalist_len :: Ptr AuichDmalist -> IO (Ptr Word32)
p_AuichDmalist_len p = return $ plusPtr p $ offsetOf_AuichDmalist_len

newtype {-# CTYPE "struct auich_cdata" #-} AuichCdata = AuichCdata ()
foreign import primitive "const.sizeof(struct auich_cdata)"
  sizeOf_AuichCdata :: Int
foreign import primitive "const.offsetof(struct auich_cdata, ic_dmalist_pcmo)"
  offsetOf_AuichCdata_ic_dmalist_pcmo :: Int
p_AuichCdata_ic_dmalist_pcmo :: Ptr AuichCdata -> Int -> IO (Ptr AuichDmalist)
p_AuichCdata_ic_dmalist_pcmo p i = return $ plusPtr p $ offsetOf_AuichCdata_ic_dmalist_pcmo + i * sizeOf_AuichDmalist
foreign import primitive "const.offsetof(struct auich_cdata, ic_dmalist_pcmi)"
  offsetOf_AuichCdata_ic_dmalist_pcmi :: Int
p_AuichCdata_ic_dmalist_pcmi :: Ptr AuichCdata -> Int -> IO (Ptr AuichDmalist)
p_AuichCdata_ic_dmalist_pcmi p i = return $ plusPtr p $ offsetOf_AuichCdata_ic_dmalist_pcmi + i * sizeOf_AuichDmalist
foreign import primitive "const.offsetof(struct auich_cdata, ic_dmalist_mici)"
  offsetOf_AuichCdata_ic_dmalist_mici :: Int
p_AuichCdata_ic_dmalist_mici :: Ptr AuichCdata -> Int -> IO (Ptr AuichDmalist)
p_AuichCdata_ic_dmalist_mici p i = return $ plusPtr p $ offsetOf_AuichCdata_ic_dmalist_mici + i * sizeOf_AuichDmalist
f_ICH_PCMO_OFF i = offsetOf_AuichCdata_ic_dmalist_pcmo + i * sizeOf_AuichDmalist
f_ICH_PCMI_OFF i = offsetOf_AuichCdata_ic_dmalist_pcmi + i * sizeOf_AuichDmalist
f_ICH_MICI_OFF i = offsetOf_AuichCdata_ic_dmalist_mici + i * sizeOf_AuichDmalist

kernAddr_AuichDma :: Ptr AuichDma -> IO (Ptr ())
kernAddr_AuichDma p = fmap castPtr $ peek =<< p_AuichDma_addr p

dmaAddr :: Ptr AuichDma -> IO BusAddrT
dmaAddr p = do
  dmaMapAddr =<< peek =<< p_AuichDma_map p

dmaMapAddr :: BusDmamapT -> IO BusAddrT
dmaMapAddr p = do
  busdmaseg <- p_BusDmamap_dm_segs p 0
  peek =<< p_BusDmaSegment_ds_addr busdmaseg
