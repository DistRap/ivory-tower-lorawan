{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.LoraWAN.Types.MType where

import Ivory.Language

[ivory|
 bitdata MType :: Bits 3
  = join_request    as 0b000
  | join_accept     as 0b001
  | uncnf_data_up   as 0b010
  | uncnf_data_down as 0b011
  | cnf_data_up     as 0b100
  | cnf_data_down   as 0b101
  | rejoin_request  as 0b110
  | proprietary     as 0b111
|]
