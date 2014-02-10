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
import Sys.Types
import Sys.Errno
import Sys.Audioio
import Sys.Bus
import Sys.Device
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

foreign import ccall "hs_extern.h get_auich_spdif_formats"
  c_get_auich_spdif_formats :: IO (Ptr AudioFormat)

foreign import primitive "const.AUICH_AUDIO_NFORMATS" e_AUICH_AUDIO_NFORMATS :: Int
foreign import primitive "const.AUICH_MODEM_NFORMATS" e_AUICH_MODEM_NFORMATS :: Int
foreign import primitive "const.AUICH_SPDIF_NFORMATS" e_AUICH_SPDIF_NFORMATS :: Int
foreign import primitive "const.ICH_CODECIO_INTERVAL" e_ICH_CODECIO_INTERVAL :: CUInt

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

newtype {-# CTYPE "struct auich_ring" #-} AuichRing = AuichRing ()
foreign import primitive "const.sizeof(struct auich_ring)"
  sizeOf_AuichRing :: Int
foreign import primitive "const.offsetof(struct auich_ring, intr)"
  offsetOf_AuichRing_intr :: Int
p_AuichRing_intr :: Ptr AuichRing -> IO (Ptr (FunPtr ((Ptr ()) -> IO ())))
p_AuichRing_intr p = return $ plusPtr p $ offsetOf_AuichRing_intr
foreign import ccall "dynamic" call_AuichRing_intr ::
  FunPtr ((Ptr ()) -> IO ()) -> (Ptr ()) -> IO ()
