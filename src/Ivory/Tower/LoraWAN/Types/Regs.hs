{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.LoraWAN.Types.Regs where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.LoraWAN.Types.Major
import Ivory.Tower.LoraWAN.Types.MType

[ivory|
 bitdata MHDR :: Bits 8 = mhdr_reg
  { mtype :: MType
  , _     :: Bits 3
  , major :: Major
  }

 bitdata DLSettings :: Bits 8 = dl_settings_reg
  { opt_neg       :: Bit     -- set means LoraWAN 1.1, unset 1.0
  , rx1_dr_offset :: Bits 3
  , rx2_data_rate :: Bits 4
  }

 bitdata MACDeviceCommand :: Bits 8
  = reset_ind                  as 0x01
  | link_check_request         as 0x02
  | link_adr_answer            as 0x03
  | duty_cycle_answer          as 0x04
  | rx_param_setup_answer      as 0x05
  | dev_status_answer          as 0x06
  | new_channel_answer         as 0x07
  | rx_timing_setup_answer     as 0x08
  | tx_param_setup_answer      as 0x09
  | dic_channel_answer         as 0x0A
  | rekey_ind                  as 0x0B
  | adr_param_setup_answer     as 0x0C
  | device_time_request        as 0x0D
 -- | _ as 0x0E
  | rejoin_param_setup_answer as 0x0F

 bitdata MACGatewayCommand :: Bits 8
  = reset_conf                 as 0x01
  | link_check_answer          as 0x02
  | link_adr_request           as 0x03
  | duty_cycle_request         as 0x04
  | rx_param_setup_request     as 0x05
  | dev_status_request         as 0x06
  | nw_channel_request         as 0x07
  | rx_timing_setup_request    as 0x08
  | tx_param_setup_request     as 0x09
  | dic_channel_request        as 0x0A
  | rekey_conf                 as 0x0B
  | adr_param_setup_request    as 0x0C
  | device_time_answer         as 0x0D
  | force_rejoin_request       as 0x0E
  | rejoin_param_setup_request as 0x0F

 bitdata FCtrlDown :: Bits 8 = fctrl_down_reg
  { fctrl_down_adr        :: Bit
  , _                     :: Bit
  , fctrl_down_ack        :: Bit
  , fctrl_down_f_pending  :: Bit    -- network has more data pending to be sent
  , fctrl_down_f_opts_len :: Bits 4
  }

 bitdata FCtrlUp :: Bits 8 = fctrl_up_reg
  { fctrl_up_adr          :: Bit
  , fctrl_up_adr_ack_req  :: Bit
  , fctrl_up_ack          :: Bit
  , fctrl_up_class_b      :: Bit
  , fctrl_up_f_opts_len   :: Bits 4
  }
|]

wrappedMHDR :: WrappedPackRep ('Stored MHDR)
wrappedMHDR = wrapPackRep "mhdr" (repackV fromRep toRep (packRep :: PackRep ('Stored Uint8)))
instance Packable ('Stored MHDR) where
  packRep = wrappedPackRep wrappedMHDR

wrappedDLSettings :: WrappedPackRep ('Stored DLSettings)
wrappedDLSettings = wrapPackRep "dlSettings" (repackV fromRep toRep (packRep :: PackRep ('Stored Uint8)))
instance Packable ('Stored DLSettings) where
  packRep = wrappedPackRep wrappedDLSettings
