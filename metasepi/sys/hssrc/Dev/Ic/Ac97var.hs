{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Ic.Ac97var where
import Data.Bits
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Sys.Audioio
import Dev.Ic.Ac97reg

foreign import primitive "const.AC97_CODEC_TYPE_AUDIO" e_AC97_CODEC_TYPE_AUDIO :: Int
foreign import primitive "const.AC97_CODEC_TYPE_MODEM" e_AC97_CODEC_TYPE_MODEM :: Int

f_AC97_IS_FIXED_RATE :: Ptr Ac97CodecIf -> IO Bool
f_AC97_IS_FIXED_RATE codec = do
  g <- peek =<< p_Ac97CodecIfVtbl_get_extcaps =<< peek =<< p_Ac97CodecIf_vtbl codec
  r <- call_Ac97CodecIfVtbl_get_extcaps g codec
  return $ if r .&. fromIntegral e_AC97_EXT_AUDIO_VRA == 0 then True else False

newtype {-# CTYPE "struct ac97_codec_if" #-} Ac97CodecIf = Ac97CodecIf ()
foreign import primitive "const.offsetof(struct ac97_codec_if, vtbl)"
  offsetOf_Ac97CodecIf_vtbl :: Int
p_Ac97CodecIf_vtbl :: Ptr Ac97CodecIf -> IO (Ptr (Ptr Ac97CodecIfVtbl))
p_Ac97CodecIf_vtbl p = return $ plusPtr p offsetOf_Ac97CodecIf_vtbl

newtype {-# CTYPE "struct ac97_codec_if_vtbl" #-} Ac97CodecIfVtbl = Ac97CodecIfVtbl ()
foreign import primitive "const.sizeof(struct ac97_codec_if_vtbl)"
  sizeOf_Ac97CodecIfVtbl :: Int
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, lock)"
  offsetOf_Ac97CodecIfVtbl_lock :: Int
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, unlock)"
  offsetOf_Ac97CodecIfVtbl_unlock :: Int
type Ac97CodecIfVtbl_lock = Ptr Ac97CodecIf -> IO ()
type Ac97CodecIfVtbl_unlock = Ptr Ac97CodecIf -> IO ()
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_lock ::
  FunPtr (Ac97CodecIfVtbl_lock) -> Ac97CodecIfVtbl_lock
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_unlock ::
  FunPtr (Ac97CodecIfVtbl_unlock) -> Ac97CodecIfVtbl_unlock

p_Ac97CodecIfVtbl_lock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_lock))
p_Ac97CodecIfVtbl_lock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_lock
p_Ac97CodecIfVtbl_unlock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_unlock))
p_Ac97CodecIfVtbl_unlock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_unlock

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, set_clock)"
  offsetOf_Ac97CodecIfVtbl_set_clock :: Int
p_Ac97CodecIfVtbl_set_clock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> CUInt -> IO ())))
p_Ac97CodecIfVtbl_set_clock p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_set_clock
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_set_clock ::
  FunPtr ((Ptr Ac97CodecIf) -> CUInt -> IO ()) -> (Ptr Ac97CodecIf) -> CUInt -> IO ()

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, set_rate)"
  offsetOf_Ac97CodecIfVtbl_set_rate :: Int
p_Ac97CodecIfVtbl_set_rate :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> Int -> Ptr CUInt -> IO Int)))
p_Ac97CodecIfVtbl_set_rate p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_set_rate
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_set_rate ::
  FunPtr ((Ptr Ac97CodecIf) -> Int -> Ptr CUInt -> IO Int) -> (Ptr Ac97CodecIf) -> Int -> Ptr CUInt -> IO Int

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, mixer_get_port)"
  offsetOf_Ac97CodecIfVtbl_mixer_get_port :: Int
p_Ac97CodecIfVtbl_mixer_get_port :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int)))
p_Ac97CodecIfVtbl_mixer_get_port p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_mixer_get_port
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_mixer_get_port ::
  FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int) -> (Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, mixer_set_port)"
  offsetOf_Ac97CodecIfVtbl_mixer_set_port :: Int
p_Ac97CodecIfVtbl_mixer_set_port :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int)))
p_Ac97CodecIfVtbl_mixer_set_port p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_mixer_set_port
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_mixer_set_port ::
  FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int) -> (Ptr Ac97CodecIf) -> Ptr MixerCtrl -> IO Int

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, query_devinfo)"
  offsetOf_Ac97CodecIfVtbl_query_devinfo :: Int
p_Ac97CodecIfVtbl_query_devinfo :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerDevinfo -> IO Int)))
p_Ac97CodecIfVtbl_query_devinfo p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_query_devinfo
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_query_devinfo ::
  FunPtr ((Ptr Ac97CodecIf) -> Ptr MixerDevinfo -> IO Int) -> (Ptr Ac97CodecIf) -> Ptr MixerDevinfo -> IO Int

foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, get_extcaps)"
  offsetOf_Ac97CodecIfVtbl_get_extcaps :: Int
p_Ac97CodecIfVtbl_get_extcaps :: Ptr Ac97CodecIfVtbl ->  IO (Ptr (FunPtr ((Ptr Ac97CodecIf) -> IO Word16)))
p_Ac97CodecIfVtbl_get_extcaps p = return $ plusPtr p $ offsetOf_Ac97CodecIfVtbl_get_extcaps
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_get_extcaps ::
  FunPtr ((Ptr Ac97CodecIf) -> IO Word16) -> (Ptr Ac97CodecIf) -> IO Word16
