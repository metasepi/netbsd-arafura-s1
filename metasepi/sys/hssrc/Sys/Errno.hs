{-# LANGUAGE ForeignFunctionInterface #-}
module Sys.Errno where

infixl 1  >>?, >>=?
infixr 1  ?<<, ?=<<

(>>=?) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
(>>=?) x f = x >>= go
  where go (Left e)  = return $ Left e
        go (Right a) = f a

(>>?) :: IO (Either e a) -> IO (Either e b) -> IO (Either e b)
(>>?) x k = x >>=? const k

(?=<<) :: (a -> IO (Either e b)) -> IO (Either e a) -> IO (Either e b)
(?=<<) f x = (>>=?) x f

(?<<) :: IO (Either e b) -> IO (Either e a) -> IO (Either e b)
(?<<) k x = (>>?) x k

foreign import primitive "const.EINVAL" e_EINVAL :: Int
