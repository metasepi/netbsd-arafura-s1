{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.AudioIf where
import Foreign.C.Types
import Foreign.Ptr

newtype {-# CTYPE "struct audio_params" #-} AudioParams = AudioParams ()
type AudioParamsT = AudioParams

foreign import primitive "const.offsetof(struct audio_params, channels)"
  offsetOf_AudioParamsT_channels :: Int
p_AudioParamsT_channels :: Ptr AudioParamsT -> IO (Ptr CUInt)
p_AudioParamsT_channels p = return $ plusPtr p offsetOf_AudioParamsT_channels

foreign import primitive "const.offsetof(struct audio_params, sample_rate)"
  offsetOf_AudioParams_sample_rate :: Int
p_AudioParams_sample_rate :: Ptr AudioParams -> IO (Ptr CUInt)
p_AudioParams_sample_rate p = return $ plusPtr p $ offsetOf_AudioParams_sample_rate

newtype {-# CTYPE "struct stream_filter_list" #-} StreamFilterList = StreamFilterList ()
type StreamFilterListT = StreamFilterList
foreign import primitive "const.sizeof(struct stream_filter_list)"
  sizeOf_StreamFilterList :: Int

foreign import primitive "const.offsetof(struct stream_filter_list, req_size)"
  offsetOf_StreamFilterList_req_size :: Int
p_StreamFilterList_req_size :: Ptr StreamFilterList -> IO (Ptr Int)
p_StreamFilterList_req_size p = return $ plusPtr p $ offsetOf_StreamFilterList_req_size

foreign import primitive "const.offsetof(struct stream_filter_list, filters)"
  offsetOf_StreamFilterList_filters :: Int
p_StreamFilterList_filters :: Ptr StreamFilterList -> Int -> IO (Ptr StreamFilterReq)
p_StreamFilterList_filters p i = return $ plusPtr p $ offsetOf_StreamFilterList_filters + i * sizeOf_StreamFilterReq

newtype {-# CTYPE "struct stream_filter_req" #-} StreamFilterReq = StreamFilterReq ()
foreign import primitive "const.sizeof(struct stream_filter_req)"
  sizeOf_StreamFilterReq :: Int

foreign import primitive "const.offsetof(struct stream_filter_req, param)"
  offsetOf_StreamFilterReq_param :: Int
p_StreamFilterReq_param :: Ptr StreamFilterReq -> IO (Ptr audio_params_t)
p_StreamFilterReq_param p = return $ plusPtr p $ offsetOf_StreamFilterReq_param
