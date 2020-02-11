{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.LoraWAN.Import where

import Ivory.Language
import Ivory.Tower.LoraWAN.Types

header :: String
header = "lorawan-crypto.h"

aes :: ConstRef s1 AESKey
    -> ConstRef s2 ('Array 16 ('Stored Uint8))
    -> Ref      s3 ('Array 16 ('Stored Uint8))
    -> Ivory eff ()
aes key plain enc = call_ aes_enc_proc key plain enc

aes_enc_proc :: Def('[ ConstRef s1 AESKey
                     , ConstRef s2 ('Array 16 ('Stored Uint8))
                     , Ref      s3 ('Array 16 ('Stored Uint8))
                     ] ':-> ())
aes_enc_proc = importProc "aes_enc" header

aes_buf :: ConstRef s1 AESKey
    -> ConstRef s2 AESArray
    -> ConstRef s3 ('Stored Uint8)
    -> Ref      s4 AESArray
    -> Ivory eff ()
aes_buf key plain len enc = call_ aes_buf_proc key plain len enc

aes_buf_proc :: Def('[ ConstRef s1 AESKey
                     , ConstRef s2 AESArray
                     , ConstRef s3 ('Stored Uint8)
                     , Ref      s4 AESArray
                     ] ':-> ())
aes_buf_proc = importProc "aes_buf" header

cmac :: ConstRef s1 AESKey
     -> ConstRef s2 CMACBuffer     -- input
     -> ConstRef s2 ('Stored Uint8) -- actual length of data in array
     -> Ref      s3 CMACArray      -- output
     -> Ivory eff ()
cmac key buf len out = call_ cmac_proc key buf len out

cmac_proc :: Def('[ ConstRef s1 AESKey
                  , ConstRef s2 CMACBuffer
                  , ConstRef s2 ('Stored Uint8)
                  , Ref      s3 CMACArray
                  ] ':-> ())
cmac_proc = importProc "cmac" header


