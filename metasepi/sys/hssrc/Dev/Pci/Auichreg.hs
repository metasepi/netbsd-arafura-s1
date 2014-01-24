{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.Auichreg where
import Sys.Bus

foreign import primitive "const.ICH_GCTRL" e_ICH_GCTRL :: BusSizeT
