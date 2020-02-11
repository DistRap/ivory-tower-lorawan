{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.LoraWAN.Types.Structs where

import Ivory.Language

import Ivory.Tower.LoraWAN.Types.Regs

-- 4 bytes as in join accept msg
type DevAddr = Uint32
-- 8 bytes - full device address with network id, unused
type DevAddrFull = Uint64

-- 8 bytes
type JoinEUI = Uint64
type DevEUI  = Uint64
type Nonce   = Uint16
type FCNT    = Uint32 -- full FCNT stored on device

type AESKey     = 'Array  16 ('Stored Uint8)
type AESArray   = 'Array 128 ('Stored Uint8)
type CMACBuffer = 'Array 128 ('Stored Uint8)
type CMACArray  = 'Array   4 ('Stored Uint8)


[ivory|
 struct phy_payload
  { phy_mhdr        :: Stored MHDR  -- 1 byte
  ; phy_mac_payload :: Struct mac_payload
  ; phy_mic         :: Array 4 (Stored Uint8)
  }

 struct mac_payload
  { mac_fhdr        :: Struct fhdr
  ; mac_fport       :: Stored Uint8
  ; mac_frm_payload :: FRMPayload
  }

 string struct FRMPayload 128

 struct fhdr
  { fhdr_dev_addr   :: Stored Uint32 -- 4 bytes 2 network prefix + 2 device
  ; fhdr_fctrl      :: Stored Uint8  -- either FCtrlUp or Down
  ; fhdr_fcnt       :: Stored Uint16
  ; fhdr_fopts      :: Array 15 (Stored Uint8) -- f_opts_len is part of fctrl
  }

 struct join_request_message
  { req_join_eui    :: Stored JoinEUI
  ; req_dev_eui     :: Stored DevEUI
  ; req_dev_nonce   :: Array 2 (Stored Uint8)
  }

 struct join_accept_message
  { acc_join_nonce  :: Array 3 (Stored Uint8)
  ; acc_home_net_id :: Array 3 (Stored Uint8)
  ; acc_dev_addr    :: Stored DevAddr
  ; acc_dl_settings :: Stored DLSettings
  ; acc_rx_delay    :: Uint8
  ; acc_cflist      :: Array 16 (Stored Uint8) -- channel frequency list
  ; acc_has_cflist  :: IBool
  }
|]
