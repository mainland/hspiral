-- |
-- Module      :  SPL.Backend.C.Code
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Backend.C.Code (
    Code(..)
  ) where

import Data.Foldable (toList)
import Data.Monoid
import Data.Sequence (Seq)
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

-- | Contains generated code.
data Code = Code
    { -- | Top-level definitions
      codeDefs :: !(Seq C.Definition)
    , -- | Top-level function definitions
      codeFunDefs :: !(Seq C.Definition)
    , -- | Local declarations
      codeDecls :: !(Seq C.InitGroup)
    , -- | Local statements
      codeStms :: !(Seq C.Stm)
    }
  deriving (Eq, Ord, Show)

instance Pretty Code where
    ppr c = stack $
            (map ppr . toList . codeDefs) c ++
            (map ppr . toList . codeFunDefs) c ++
            (map ppr . toList . codeDecls) c ++
            (map ppr . toList . codeStms) c

instance Monoid Code where
    mempty = Code
        { codeDefs              = mempty
        , codeFunDefs           = mempty
        , codeDecls             = mempty
        , codeStms              = mempty
        }

    a `mappend` b = Code
        { codeDefs    = codeDefs a <> codeDefs b
        , codeFunDefs = codeFunDefs a <> codeFunDefs b
        , codeDecls   = codeDecls a <> codeDecls b
        , codeStms    = codeStms a <> codeStms b
        }
