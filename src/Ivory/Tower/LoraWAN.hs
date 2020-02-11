{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.LoraWAN (
    new
  , joinRequest
  , fromJoinAccept
  , uplink
  , packString
  , loraWANDeps
  , loraWANModule
  , module Ivory.Tower.LoraWAN.Types
  ) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower

import GHC.TypeLits

import Ivory.Tower.LoraWAN.Artifacts
import Ivory.Tower.LoraWAN.Types
import Ivory.Tower.LoraWAN.Keys
import Ivory.Tower.LoraWAN.Import
import Ivory.Tower.LoraWAN.Pack

new :: (GetAlloc eff ~ 'Scope s)
    => MType
    -> Ivory eff (Ref ('Stack s) ('Struct "phy_payload"))
new mtyp = do
  mhdr <- assign $ fromRep $ withBits 0 $ do
      setField mtype mtyp
      setField major lorawan_r1

  phy <- local $ izero
  store (phy ~> phy_mhdr) mhdr
  return phy

packString :: (IvoryString ('Struct str), SignCast from Sint32)
           => Ref s ('Struct str)
           -> (Ref s ('Array (Capacity ('Struct str)) ('Stored Uint8)) -> Ivory eff from)
           -> Ivory eff ()
packString str packf = do
  len <- packf (str ~> stringDataL)
  store (str ~> stringLengthL) $ signCast len

joinRequest :: (IvoryString str)
            => Ref s str
            -> ConstRef s1 KeyNWK
            -> JoinEUI
            -> DevEUI
            -> Uint16
            -> Ivory (AllocEffects s6) ()
joinRequest buf nwkKey appEui devEui nonce = do
  join <- local $ istruct [
         req_join_eui .= ival appEui
       , req_dev_eui  .= ival devEui
       , req_dev_nonce .= iarray (map ival [bitCast $ nonce `iShiftL` 8, bitCast nonce])
       ]

  msg <- new join_request
  packString (msg ~> phy_mac_payload ~> mac_frm_payload) $ \packTo ->
    packJoinRequest packTo 0 (constRef join)

  micJoinRequest nwkKey msg (constRef join)
  packString buf $ flip packPhyPayload msg

uplink :: (IvoryString str)
       => Ref s str
       -> ConstRef s1 KeyAppS
       -> ConstRef s1 KeyFNwkSInt
       -> IBool                        -- confirmed message
       -> Uint8                        -- FPort
       -> ConstRef s2 ('Stored DevAddr) -- Device address
       -> ConstRef s3 ('Stored FCNT)   -- counter
       -> ConstRef s4 AESArray         -- plain text payload
       -> ConstRef s5 ('Stored Uint8)  -- payload length
       -> Ivory (AllocEffects s6) ()
uplink buf appSKey nwkSKey isConfirmed fport devAddr fcnt payload len = do
  msg <- new (isConfirmed ? (cnf_data_up, uncnf_data_up))
  store (msg ~> phy_mac_payload ~> mac_fport) fport
  refCopy (msg ~> phy_mac_payload ~> mac_fhdr ~> fhdr_dev_addr) devAddr
  fcnt16 <- fmap bitCast $ deref fcnt
  store (msg ~> phy_mac_payload ~> mac_fhdr ~> fhdr_fcnt) fcnt16

  packString (msg ~> phy_mac_payload ~> mac_frm_payload) $ \to ->
    encryptPayload to false appSKey devAddr fcnt payload len

  micData false nwkSKey devAddr fcnt msg
  packString buf $ flip packPhyPayload msg


fromJoinAccept :: KnownNat len
               => ConstRef s1 ('Array len ('Stored Uint8))
               -> ConstRef s2 KeyNWK
               -> Uint32
               -> Ivory (AllocEffects s3)
                    (ConstRef ('Stack s3) ('Stored IBool),
                     ConstRef ('Stack s3) ('Struct "join_accept_message"))
fromJoinAccept buf nwkKey len = do
  p <- unpackPhyPayload buf len
  l <- deref (p ~> phy_mac_payload ~> mac_frm_payload ~> stringLengthL)
  lenU8 <- local $ ival $ (bitCast :: Uint32 -> Uint8) $ signCast l

  acc <- decryptJoinAccept nwkKey
    (constRef $ p ~> phy_mac_payload ~> mac_frm_payload ~> stringDataL)
    (constRef $ lenU8)

  mic <- micJoinAccept nwkKey (buf ! 0) acc
  micValid <- local $ ival true
  arrayMap $ \i -> do
    computed <- deref (mic ! i)
    received <- deref (p ~> phy_mic ! i)
    unless (computed ==? received) $ do
      store micValid false
      breakOut

  return (constRef micValid, acc)

loraWANModule :: Module
loraWANModule = package "lorawan" $ do
  depend serializeModule
  defStringType (Proxy :: Proxy FRMPayload)
  defStruct (Proxy :: Proxy "join_request_message")
  defStruct (Proxy :: Proxy "join_accept_message")
  defStruct (Proxy :: Proxy "fhdr")
  defStruct (Proxy :: Proxy "mac_payload")
  defStruct (Proxy :: Proxy "phy_payload")

  incl cmac_proc
  incl aes_enc_proc
  incl aes_buf_proc

  wrappedPackMod wrappedMHDR
  wrappedPackMod wrappedDLSettings

loraWANDeps :: Tower e ()
loraWANDeps = do
  mapM_ towerArtifact loraWANArtifacts
  mapM_ towerArtifact serializeArtifacts

  towerDepends loraWANModule
  towerModule  loraWANModule

  towerDepends serializeModule
  towerModule  serializeModule
