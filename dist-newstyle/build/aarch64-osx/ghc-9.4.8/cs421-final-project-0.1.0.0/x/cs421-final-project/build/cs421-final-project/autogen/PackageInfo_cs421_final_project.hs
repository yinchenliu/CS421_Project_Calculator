{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cs421_final_project (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cs421_final_project"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A brief description of your project"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
