module Sys.Proc where

newtype {-# CTYPE "struct proc" #-} Proc = Proc ()
foreign import primitive "const.sizeof(struct proc)"
  sizeOf_Proc :: Int
