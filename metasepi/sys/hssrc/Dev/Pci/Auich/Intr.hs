module Dev.Pci.Auich.Intr where
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Kern.KernMutex
import Kern.SubrPrf
import Sys.Bus
import Sys.Device
import Dev.Pci.Auichreg
import Dev.Ic.Ac97var
import Dev.Pci.Auich.Ptr

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

foreign export ccall "auichIntr"
  auichIntr :: Ptr AuichSoftc -> IO Int
auichIntr :: Ptr AuichSoftc -> IO Int
auichIntr sc = do
  pw <- deviceHasPower =<< peek =<< p_AuichSoftc_sc_dev sc
  if pw then auichIntr' sc else return 0
auichIntr' :: Ptr AuichSoftc -> IO Int
auichIntr' sc = do
  mutexp <- p_AuichSoftc_sc_intr_lock sc
  mutexSpinEnter mutexp
  codectype <- peek =<< p_AuichSoftc_sc_codectype sc
  iot <- peek =<< p_AuichSoftc_iot sc
  aud_ioh <- peek =<< p_AuichSoftc_aud_ioh sc
  modem_offset <- peek =<< p_AuichSoftc_sc_modem_offset sc
  gsts <- fmap fromIntegral $ busSpaceRead4 iot aud_ioh $ e_ICH_GSTS + modem_offset
  let f1, f2, f3, post :: Int -> IO Int
      f1 r =
        if (codectype == e_AC97_CODEC_TYPE_AUDIO && gsts .&. e_ICH_POINT /= 0) ||
	   (codectype == e_AC97_CODEC_TYPE_MODEM && gsts .&. e_ICH_MOINT /= 0) then do
          stsReg <- peek =<< p_AuichSoftc_sc_sts_reg sc
          sts <- busSpaceRead2 iot aud_ioh $ fromIntegral e_ICH_PCMO + fromIntegral stsReg
          when (sts .&. e_ICH_FIFOE /= 0) $ do
            printfS1 "%s: fifo overrun\n" =<< deviceXname =<< peek =<< p_AuichSoftc_sc_dev sc
          when (sts .&. e_ICH_BCIS /= 0) $ do
            auichIntrPipe sc e_ICH_PCMO =<< p_AuichSoftc_pcmo sc
          -- int ack
          busSpaceWrite2 iot aud_ioh (fromIntegral e_ICH_PCMO + fromIntegral stsReg)
            (sts .&. (e_ICH_BCIS .|. e_ICH_FIFOE))
          if codectype == e_AC97_CODEC_TYPE_AUDIO then
            busSpaceWrite4 iot aud_ioh (e_ICH_GSTS + modem_offset) $ fromIntegral e_ICH_POINT
          else
            busSpaceWrite4 iot aud_ioh (e_ICH_GSTS + modem_offset) $ fromIntegral e_ICH_MOINT
          return $ r + 1
        else
          return r
      f2 r =
        if (codectype == e_AC97_CODEC_TYPE_AUDIO && gsts .&. e_ICH_PIINT /= 0) ||
	   (codectype == e_AC97_CODEC_TYPE_MODEM && gsts .&. e_ICH_MIINT /= 0) then do
          stsReg <- peek =<< p_AuichSoftc_sc_sts_reg sc
          sts <- busSpaceRead2 iot aud_ioh $ fromIntegral e_ICH_PCMI + fromIntegral stsReg
          when (sts .&. e_ICH_FIFOE /= 0) $ do
            printfS1 "%s: fifo overrun\n" =<< deviceXname =<< peek =<< p_AuichSoftc_sc_dev sc
          when (sts .&. e_ICH_BCIS /= 0) $ do
            auichIntrPipe sc e_ICH_PCMI =<< p_AuichSoftc_pcmi sc
          -- int ack
          busSpaceWrite2 iot aud_ioh (fromIntegral e_ICH_PCMI + fromIntegral stsReg)
            (sts .&. (e_ICH_BCIS .|. e_ICH_FIFOE))
          if codectype == e_AC97_CODEC_TYPE_AUDIO then
            busSpaceWrite4 iot aud_ioh (e_ICH_GSTS + modem_offset) $ fromIntegral e_ICH_PIINT
          else
            busSpaceWrite4 iot aud_ioh (e_ICH_GSTS + modem_offset) $ fromIntegral e_ICH_MIINT
          return $ r + 1
        else
          return r
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
        mutexSpinExit mutexp
        return r
  f1 0 >>= f2 >>= f3 >>= post
