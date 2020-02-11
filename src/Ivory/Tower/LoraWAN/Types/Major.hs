{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.LoraWAN.Types.Major where

import Ivory.Language

[ivory|

bitdata Major :: Bits 2
  = lorawan_r1 as 0b00
 -- | reserved   as 0b01
 -- | reserved   as 0b10
 -- | reserved   as 0b11
|]
