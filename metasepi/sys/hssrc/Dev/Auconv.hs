{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Auconv where
import Foreign.C.Types
import Foreign.Ptr
import Dev.AudioIf

auconvQueryEncoding :: Ptr AudioEncodingSet -> Ptr AudioEncodingT -> IO Int
auconvQueryEncoding = c_auconv_query_encoding
foreign import ccall "hs_extern.h auconv_query_encoding"
  c_auconv_query_encoding :: Ptr AudioEncodingSet -> Ptr AudioEncodingT -> IO Int

auconvSetConverter :: Ptr AudioFormat -> Int -> Int -> Ptr AudioParams -> Int -> Ptr StreamFilterList -> IO Int
auconvSetConverter = c_auconv_set_converter
foreign import ccall "hs_extern.h auconv_set_converter"
  c_auconv_set_converter :: Ptr AudioFormat -> Int -> Int -> Ptr AudioParams -> Int -> Ptr StreamFilterList -> IO Int

--------------------------------------------------------------------------
-- Following code is generated by struct2hs command semi-automatically. --
--------------------------------------------------------------------------
newtype {-# CTYPE "audio_encoding_set" #-} AudioEncodingSet = AudioEncodingSet ()
newtype {-# CTYPE "audio_encoding_t" #-} AudioEncodingT = AudioEncodingT ()
newtype {-# CTYPE "struct audio_format" #-} AudioFormat = AudioFormat ()
foreign import primitive "const.sizeof(struct audio_format)"
  sizeOf_AudioFormat :: Int
foreign import primitive "const.offsetof(struct audio_format, frequency_type)"
  offsetOf_AudioFormat_frequency_type :: Int
p_AudioFormat_frequency_type :: Ptr AudioFormat -> IO (Ptr CUInt)
p_AudioFormat_frequency_type p = return $ plusPtr p $ offsetOf_AudioFormat_frequency_type
