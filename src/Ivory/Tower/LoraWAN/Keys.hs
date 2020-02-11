{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.LoraWAN.Keys where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.LoraWAN.Types
import Ivory.Tower.LoraWAN.Import

-- toplevel
type KeyNWK = AESKey
type KeyApp = AESKey

-- derived
type KeyFNwkSInt = AESKey
type KeyAppS     = AESKey
type KeySNwkSInt = AESKey
type KeyNwkSEnc  = AESKey
type KeyJsEnc    = AESKey
type KeyJsInt    = AESKey

keyFNwkSInt :: Uint8
keyFNwkSInt = 0x01
keyAppS     :: Uint8
keyAppS     = 0x02
keySNwkSInt :: Uint8
keySNwkSInt = 0x03
keyNwkSEnc  :: Uint8
keyNwkSEnc  = 0x04
keyJsEnc    :: Uint8
keyJsEnc    = 0x05
keyJsInt    :: Uint8
keyJsInt    = 0x06

type KDFJoin fromKey toKey = forall s s1 s2 s3 . ConstRef s1 fromKey
          -> ConstRef s2 ('Stored Uint16)
          -> ConstRef s3 ('Struct "join_accept_message")
          -> Ivory (AllocEffects s) (ConstRef ('Stack s) toKey)

type KDFDev fromKey toKey = forall s s1 s2 . ConstRef s1 fromKey
          -> ConstRef s2 ('Stored DevEUI)
          -> Ivory (AllocEffects s) (ConstRef ('Stack s) toKey)

-- for 1.0
-- AppSKey     = aes128_encrypt(NwkKey, 0x02 | JoinNonce | NetID | DevNonce | pad 161 )
-- FNwkSIntKey = aes128_encrypt(NwkKey, 0x01 | JoinNonce | NetID | DevNonce | pad 16 )
-- SNwkSIntKey = NwkSEncKey = FNwkSIntKey

-- for 1.1 ...
-- this one is common
deriveKeyFNwkSInt :: KDFJoin KeyNWK KeyFNwkSInt
deriveKeyFNwkSInt = deriveKey keyFNwkSInt

-- in case of 1.0 this is KDFJoin KeyNWK KeyAppS
deriveKeyAppS :: KDFJoin KeyApp KeyAppS
deriveKeyAppS = deriveKey keyAppS

deriveKeySnwkSInt :: KDFJoin KeyNWK KeySNwkSInt
deriveKeySnwkSInt = deriveKey keySNwkSInt

deriveKeyNwkSEnc :: KDFJoin KeyNWK KeyNwkSEnc
deriveKeyNwkSEnc = deriveKey keyNwkSEnc

deriveKeyJsEnc :: KDFDev KeyNWK KeyJsEnc
deriveKeyJsEnc = deriveKeyFromDevEUI keyJsEnc

deriveKeyJsInt :: KDFDev KeyNWK KeyJsInt
deriveKeyJsInt = deriveKeyFromDevEUI keyJsInt

-- derivey key from NwkKey, nonce and join_accept_message
deriveKey :: Uint8
          -> ConstRef s1 AESKey
          -> ConstRef s2 ('Stored Uint16)
          -> ConstRef s3 ('Struct "join_accept_message")
          -> Ivory (AllocEffects s) (ConstRef ('Stack s) AESKey)
deriveKey keyType key devnonce a = do
  buf <- local $ izero
  store (buf ! 0) keyType
  arrayMap $ \i ->
    packInto buf (1 + (signCast $ 2 - fromIx i)) (a ~> acc_join_nonce ! i)
  arrayMap $ \i ->
    packInto buf (1 + 3 + (signCast $ 2 - fromIx i)) (a ~> acc_home_net_id ! i)

  packInto buf 7 devnonce

  derived <- local $ izero
  aes key (constRef buf) derived
  return $ constRef derived

deriveKeyFromDevEUI :: Uint8
          -> ConstRef s1 AESKey
          -> ConstRef s2 ('Stored DevEUI)
          -> Ivory (AllocEffects s) (ConstRef ('Stack s) AESKey)
deriveKeyFromDevEUI keyType nwkKey devEUI = do
  buf <- local $ iarray $ map ival [keyType]
  packInto buf 1 devEUI

  derived <- local $ izero
  aes nwkKey (constRef buf) derived
  return $ constRef derived
