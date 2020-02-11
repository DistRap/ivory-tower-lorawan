{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.LoraWAN.Pack where


import GHC.TypeLits (KnownNat)

import Ivory.Language
import Ivory.Serialize
import qualified Ivory.Serialize.LittleEndian as LE
import Ivory.Stdlib
import Ivory.Tower.LoraWAN.Types
import Ivory.Tower.LoraWAN.Keys
import Ivory.Tower.LoraWAN.Import

packPhyPayload :: KnownNat len
               => Ref s1 ('Array len ('Stored Uint8))
               -> Ref s2 ('Struct "phy_payload")
               -> Ivory (AllocEffects s3) Uint32

packPhyPayload buf pRef = do
  p <- assign $ constRef pRef

  packInto buf 0 (p ~> phy_mhdr)
  off <- packMacPayload buf 1 (p ~> phy_mhdr) (p ~> phy_mac_payload)
  comment "pack mic"
  arrayMap $ \i -> do
    packInto buf (off + (signCast $ fromIx i))
      (p ~> phy_mic ! i)

  return $ off + 4

unpackPhyPayload :: KnownNat len
                 => ConstRef s1 ('Array len ('Stored Uint8))
                 -> Uint32
                 -> Ivory (AllocEffects s2) (Ref ('Stack s2) ('Struct "phy_payload"))
unpackPhyPayload buf len = do
  p <- local $ izero
  unpackFrom buf 0 (p ~> phy_mhdr)
  -- if this is join accept mic is part of encrypted payload
  l <- assign $ len - 5
  unpackMacPayload buf 1 l (constRef $ p ~> phy_mhdr) (p ~> phy_mac_payload)

  arrayMap $ \i -> do
    unpackFrom buf ((len - 4) + (signCast $ fromIx i))
      (p ~> phy_mic ! i)
  return p

packMacPayload :: KnownNat len
               => Ref s1 ('Array len ('Stored Uint8))
               -> Uint32
               -> ConstRef s2 ('Stored MHDR)
               -> ConstRef s3 ('Struct "mac_payload")
               -> Ivory (AllocEffects s4) Uint32
packMacPayload buf off mhdrRef m = do
  mhdr <- deref mhdrRef
  -- cond, handle empty payload but non-empty fhdr addr
  fhdrOff <- ifte (
        mhdr #. mtype ==? join_request
    .|| mhdr #. mtype ==? join_accept
    .|| mhdr #. mtype ==? rejoin_request
    )
    (return off)
    (do
      o <- packFHDR buf off (m ~> mac_fhdr)
      packInto buf o (m ~> mac_fport)
      return $ o + 1
    )

  len <- deref (m ~> mac_frm_payload ~> stringLengthL)
  comment "pack frm_payload"
  arrayMap $ \i -> do
    when (i <=? toIx len) $ do
      packInto buf (fhdrOff + (signCast $ fromIx i))
        (m ~> mac_frm_payload ~> stringDataL ! i)

  return $ fhdrOff + (signCast len)

unpackMacPayload :: KnownNat len
                 => ConstRef s1 ('Array len ('Stored Uint8))
                 -> Uint32
                 -> Uint32
                 -> ConstRef s2 ('Stored MHDR)
                 -> Ref s3 ('Struct "mac_payload")
                 -> Ivory (AllocEffects s4) ()
unpackMacPayload buf off len mhdrRef mp = do
  mhdr <- deref mhdrRef
  -- cond, handle empty payload but non-empty fhdr addr
  fhdrOff <- ifte (
        mhdr #. mtype ==? join_request
    .|| mhdr #. mtype ==? join_accept
    .|| mhdr #. mtype ==? rejoin_request
    )
    (return off)
    (do
      o <- unpackFHDR buf off (mp ~> mac_fhdr)
      unpackFrom buf o (mp ~> mac_fport)
      return $ o + 1
    )

  comment "unpack frm_payload"
  arrayMap $ \i -> do
    when (i <=? toIx ((signCast :: Uint32 -> Sint32) (len - fhdrOff))) $ do
      unpackFrom buf (fhdrOff + (signCast $ fromIx i))
        (mp ~> mac_frm_payload ~> stringDataL ! i)

  store (mp ~> mac_frm_payload ~> stringLengthL)
        (signCast $ len - (fhdrOff - off))

  return ()

packFHDR :: KnownNat len
         => Ref s1 ('Array len ('Stored Uint8))
         -> Uint32
         -> ConstRef s2 ('Struct "fhdr")
         -> Ivory (AllocEffects s) Uint32
packFHDR buf off f = do
  -- only if not zero
  hasAddr <- fmap (/=? 0) $ deref $ f ~> fhdr_dev_addr
  aOff <- assign $ hasAddr ? (4, 0)
  when hasAddr $
    LE.packInto buf off (f ~> fhdr_dev_addr)

  packInto buf (off + aOff) (f ~> fhdr_fctrl)
  LE.packInto buf (off + aOff + 1) (f ~> fhdr_fcnt)

  comment "pack fopts"
  foptsLen <- fmap (.& 0xF) $ deref (f ~> fhdr_fctrl)
  arrayMap $ \i -> do
    when (i <=? toIx foptsLen) $ do
      packInto buf (off + aOff + 1 + 2 + (signCast $ fromIx i))
        (f ~> fhdr_fopts ! i)

  return $ off + aOff + 1 + 2 + (safeCast foptsLen)

unpackFHDR :: KnownNat len
           => ConstRef s1 ('Array len ('Stored Uint8))
           -> Uint32
           -> Ref s2 ('Struct "fhdr")
           -> Ivory (AllocEffects s3) Uint32
unpackFHDR buf off f = do
  LE.unpackFrom buf off (f ~> fhdr_dev_addr)
  unpackFrom buf (off + 4) (f ~> fhdr_fctrl)
  LE.unpackFrom buf (off + 4 + 1) (f ~> fhdr_fcnt)

  comment "unpack fopts"
  foptsLen <- fmap (.& 0xF) $ deref (f ~> fhdr_fctrl)
  arrayMap $ \i -> do
    when (i <=? toIx foptsLen) $ do
      unpackFrom buf (off + 4 + 1 + 2 + (signCast $ fromIx i))
        (f ~> fhdr_fopts ! i)

  return $ off + 4 + 1 + 2 + (safeCast foptsLen)

packJoinRequest :: KnownNat len
                => Ref s1 ('Array len ('Stored Uint8))
                -> Uint32
                -> ConstRef s2 ('Struct "join_request_message")
                -> Ivory (AllocEffects s3) Uint32
packJoinRequest buf off r = do
  LE.packInto buf off (r ~> req_join_eui)
  LE.packInto buf (off + 8) (r ~> req_dev_eui)
  arrayMap $ \i ->
    packInto buf (off + 16 + (signCast $ fromIx i))
      (r ~> req_dev_nonce ! i)
  return $ off + 8 + 8 + 2

packJoinAccept :: KnownNat len
               => Ref s1 ('Array len ('Stored Uint8))
               -> Uint32
               -> ConstRef s2 ('Struct "join_accept_message")
               -> Ivory (AllocEffects s3) Uint32
packJoinAccept buf off r = do
  arrayMap $ \i ->
    packInto buf (off + (signCast $ 2 - fromIx i)) (r ~> acc_join_nonce ! i)
  arrayMap $ \i ->
    packInto buf (off + 3 + (signCast $ 2 - fromIx i)) (r ~> acc_home_net_id ! i)

  LE.packInto buf (off + 6) (r ~> acc_dev_addr)
  packInto buf (off + 10) (r ~> acc_dl_settings)
  packInto buf (off + 11) (r ~> acc_rx_delay)

  hasCF <- r ~>* acc_has_cflist
  when hasCF $ do
    comment "CFList"
    arrayMap $ \i -> do
      refCopy (buf ! (toIx $ signCast off + 12 + fromIx i)) (r ~> acc_cflist ! i)

  cfLen <- assign $ hasCF ? (16, 0)

  return $ off + 3 + 3 + 4 + 1 + 1 + cfLen

unpackJoinAccept :: KnownNat len
                 => ConstRef s1 ('Array len ('Stored Uint8))
                 -> Uint32
                 -> ConstRef s2 ('Stored Uint8)
                 -> Ivory (AllocEffects s3)
                          (ConstRef ('Stack s3) ('Struct "join_accept_message"))
unpackJoinAccept buf off lenRef = do
  a <- local $ izero

  -- both arrays are little endian in payload, hence 2 - i
  arrayMap $ \i ->
    unpackFrom buf (off + (signCast $ 2 - fromIx i))
      (a ~> acc_join_nonce ! i)

  arrayMap $ \i ->
    unpackFrom buf (off + 3 + (signCast $ 2 - fromIx i))
      (a ~> acc_home_net_id ! i)

  LE.unpackFrom buf (off + 6) (a ~> acc_dev_addr)
  unpackFrom buf (off + 10) (a ~> acc_dl_settings)
  unpackFrom buf (off + 11) (a ~> acc_rx_delay)

  len <- deref lenRef
  assert(len ==? 12 .|| len ==? 28)
  when (len ==? 28) $ do
    comment "with CFList"
    arrayMap $ \i -> do
      refCopy (a ~> acc_cflist ! i) (buf ! (toIx $ signCast off + 12 + fromIx i))
    store (a ~> acc_has_cflist) true

  return $ constRef a


micJoinRequest :: ConstRef s1 KeyNWK
               -> Ref s2 ('Struct "phy_payload")
               -> ConstRef s3 ('Struct "join_request_message")
               -> Ivory (AllocEffects s4) ()
micJoinRequest nwkKey phy r = do
  buf <- local $ izero
  mhdr' <- deref (phy ~> phy_mhdr)
  store (buf ! 0) (toRep mhdr')
  off <- packJoinRequest buf 1 r
  assert (off ==? 19)

  len <- local $ ival $ bitCast off
  cmac nwkKey (constRef buf) (constRef len) (phy ~> phy_mic)

micJoinAccept :: ConstRef s1 KeyNWK
              -> ConstRef s2 ('Stored Uint8)
              -> ConstRef s3 ('Struct "join_accept_message")
              -> Ivory (AllocEffects s4)
                       (ConstRef ('Stack s4) CMACArray)
micJoinAccept nwkKey mhdr a = do
  buf <- local $ izero
  mhdr' <- deref mhdr
  store (buf ! 0) mhdr'
  off <- packJoinAccept buf 1 a

  len <- local $ ival $ bitCast off
  mic <- local $ izero

  cmac nwkKey (constRef buf) (constRef len) mic
  return $ constRef mic

-- 1.0
-- ConfFCnt: (1.1 only)
-- If the ACK bit of the uplink frame is set, meaning this frame is acknowledging a
-- downlink “confirmed” frame, then ConfFCnt is the frame counter value modulo 2^16
-- of the “confirmed” downlink frame that is being acknowledged.

micData :: IBool
        -> ConstRef s1 KeyFNwkSInt
        -> ConstRef s2 ('Stored DevAddr)
        -> ConstRef s3 ('Stored FCNT)
        -> Ref s4 ('Struct "phy_payload")
        -> Ivory (AllocEffects s5) ()
micData isDownlink nwkSKey devAddr fcnt phy = do
  buf <- local $ izero
  store (buf ! 0) 0x49
  -- 1,2: (ConfFCnt) -- confirmed stuff
  -- 3,4: 0
  -- direction
  store (buf ! 5) (isDownlink ? (0x1, 0x0))
  LE.packInto buf 6 devAddr
  LE.packInto buf 10 fcnt
  -- 15 payload length

  packInto buf 16 (constRef $ phy ~> phy_mhdr)
  off <- packMacPayload buf 17 (constRef $ phy ~> phy_mhdr) (constRef $ phy ~> phy_mac_payload)

  -- length of MHDR + MAC PAYLOAD
  store (buf ! 15) (bitCast $ off - 16)

  len <- local $ ival $ bitCast off

  cmac nwkSKey (constRef buf) (constRef len) (phy ~> phy_mic)

decryptJoinAccept :: ConstRef s1 AESKey
                  -> ConstRef s2 AESArray
                  -> ConstRef s2 ('Stored Uint8)
                  -> Ivory (AllocEffects s3)
                       (ConstRef ('Stack s3) ('Struct "join_accept_message"))
decryptJoinAccept nwkKey encrypted len = do
  -- acc len is 12 to 12 + 16 (optional cflist) + 4 mic = 32
  out <- local $ izero
  aes_buf nwkKey encrypted len out
  ja <- unpackJoinAccept (constRef out) 0 len
  return ja

encryptPayload :: Ref s1 AESArray
               -> IBool
               -> ConstRef s2 KeyAppS
               -> ConstRef s3 ('Stored DevAddr)
               -> ConstRef s4 ('Stored FCNT)
               -> ConstRef s5 AESArray
               -> ConstRef s6 ('Stored Uint8)
               -> Ivory (AllocEffects s7) Uint32
encryptPayload buf isDownlink appSKey devAddr fcnt plain len = do
  blocks <- local $ izero
  (l :: Uint8) <- deref len
  -- this floors but we go from 0 in next
  -- loop effectively adding one block
  nBlocks <- assign $ toIx $ (l >=? 128) ? (7, (l `iDiv` 16))

  0 `upTo` (nBlocks :: Ix 8) $ \i -> do
    let ix n = toIx $ n + fromIx i * 16
        off n = signCast $ n + fromIx i * 16
    store (blocks ! ix 0) 0x1
    -- direction (0 uplink, 1 downlink)
    store (blocks ! ix 5) (isDownlink ? (0x1, 0x0))
    LE.packInto blocks (off 6) devAddr
    LE.packInto blocks (off 10) fcnt
    -- block index (1..nBlocks)
    store (blocks ! ix 15) ((bitCast :: Uint32 -> Uint8) $ signCast $ fromIx i + 1)

  aes_buf appSKey (constRef blocks) len buf
  -- XXX: xor only len bytes
  xor_buf buf plain len
  return (safeCast l)

xor_buf ::  (GetBreaks (AllowBreak eff) ~ 'Break)
        => Ref s1 AESArray
        -> ConstRef s2 AESArray
        -> ConstRef s3 ('Stored Uint8)
        -> Ivory eff ()
xor_buf a b len = do
  l <- deref len
  arrayMap $ \i -> do
    when (fromIx i >? safeCast l) breakOut
    x <- deref (a ! i)
    y <- deref (b ! i)
    store (a ! i) (x .^ y)
