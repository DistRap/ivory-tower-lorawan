{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Ivory.Artifact
import Ivory.Language

import Ivory.Tower.LoraWAN

import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tasty

import Types

joinEUI :: JoinEUI
joinEUI = 0x70B3D57ED002695F

devEUI :: DevEUI
devEUI = 0x00F9F42F46DA72DD

samplesTest :: Def ('[] :-> Sint32)
samplesTest = proc "main" $ body $ do

  (buf :: Ref s UARTBuffer) <- local $ izero
  nwkKey <- local
    $ iarray
    $ map ival [ 0xE2, 0xCD, 0x75, 0xC8, 0xBD, 0xCB, 0xE7, 0xA1, 0x5C, 0x34, 0x64, 0x48, 0x99, 0xD3, 0xDA, 0x09 ]

  noReturn $ joinRequest
    buf
    (constRef nwkKey)
    joinEUI
    devEUI
    55164

  bl <- deref (buf ~> stringLengthL)
  assert (bl ==? 23)

  -- shouldbe
  (ex :: Ref s UARTBuffer) <- local $ stringInitFromHex "005F6902D07ED5B370DD72DA462FF4F900D77CFB853B5C"
  same <- local $ ival true
  arrayMap $ \i -> do
    a <- deref (buf ~> stringDataL ! i)
    b <- deref (ex ~> stringDataL ! i)
    unless (a ==? b) $ store same false >> breakOut

  comment "same?"
  match <- deref same
  assert match

  (accept :: Ref s UARTBuffer) <- local $ stringInitFromHex "20424f36ffe6dd423ba3f49fae79febd0d5166301e143e9eb4490808537f7bff98"
  len <- accept ~>* stringLengthL
  comment "acc"
  (valid, acc) <- noReturn $ fromJoinAccept
    (constRef $ accept ~> stringDataL)
    (constRef nwkKey)
    (signCast len)

  isValid <- deref valid
  assert isValid

  devAddr <- acc ~>* acc_dev_addr
  assert (devAddr ==? 0x2601510a)
  hasCFList <- acc ~>* acc_has_cflist
  assert (hasCFList)
  --
  --  {acc_join_nonce = {0x90, 0xfd, 0x9b}, acc_home_net_id = {0x0, 0x0, 0x13}, acc_dev_addr = 0x2601510a, acc_dl_settings = 0x3, acc_rx_delay = 0x1,
  --  acc_cflist = {0x18, 0x4f, 0x84, 0xe8, 0x56, 0x84, 0xb8, 0x5e, 0x84, 0x88, 0x66, 0x84, 0x58, 0x6e, 0x84, 0x0}, acc_has_cflist = 0x1}

  ret 0

wanDeps :: [Module]
wanDeps = [uartTypes, loraWANModule, serializeModule]

wanArtifacts :: [Located Artifact]
wanArtifacts = loraWANArtifacts ++ serializeArtifacts

main :: IO ()
main = defaultMain
  $ ivoryTestGroup wanDeps wanArtifacts
  $ map (setDeps (depend uartTypes >> depend loraWANModule))
    [ mkSuccess "serialization of sample OTAA session" samplesTest ]
