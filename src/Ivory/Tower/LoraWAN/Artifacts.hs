module Ivory.Tower.LoraWAN.Artifacts
  ( loraWANArtifacts
  ) where

import Ivory.Artifact
import qualified Paths_ivory_tower_lorawan
import System.FilePath

loraWANArtifacts :: [Located Artifact]
loraWANArtifacts = map (\ p -> loc p $ src p)
  (aesFiles ++ aesModesFiles ++ apiFiles)
  where
  loc f = case takeExtension f of
    ".h" -> Incl
    ".c" -> Src
    _ -> Root
  src f = artifactCabalFile Paths_ivory_tower_lorawan.getDataDir ("support" </> f)


apiFiles, aesFiles, aesModesFiles :: [FilePath]

apiFiles =
  [ "lorawan-crypto.c"
  , "lorawan-crypto.h"
  ]

aesFiles = map ("aes" </>)
  [ "aes.h"
  , "aescrypt.c"
  , "aeskey.c"
  , "aestab.c"
  , "aestab.h"
  , "brg_endian.h"
  , "brg_types.h"
  , "mode_hdr.h"
  ]

aesModesFiles = map ("aesmodes" </>)
  [ "cmac.h"
  , "cmac.c"
  ]
