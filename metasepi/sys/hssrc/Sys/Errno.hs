{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Errno where

foreign import primitive "const.EINVAL" e_EINVAL :: Int
