{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Auconv where
import Foreign.Ptr

newtype {-# CTYPE "audio_encoding_set" #-} AudioEncodingSet = AudioEncodingSet ()
newtype {-# CTYPE "audio_encoding_t" #-} AudioEncodingT = AudioEncodingT ()

auconvQueryEncoding :: Ptr AudioEncodingSet -> Ptr AudioEncodingT -> IO Int
auconvQueryEncoding = c_auconv_query_encoding
foreign import ccall "hs_extern.h auconv_query_encoding"
  c_auconv_query_encoding :: Ptr AudioEncodingSet -> Ptr AudioEncodingT -> IO Int
