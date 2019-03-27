-- |
-- Module      :  Spiral.Backend.C.Code
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Code (
    Code(..)
  ) where

import Data.Foldable (toList)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

-- | Contains generated code.
data Code = Code
    { -- | Top-level definitions
      codeDefs :: !(Seq C.Definition)
    , -- | Top-level function definitions
      codeFunDefs :: !(Seq C.Definition)
    , -- | Function-level declarations
      codeFunDecls :: !(Seq C.InitGroup)
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
            (map ppr . toList . codeFunDecls) c ++
            (map ppr . toList . codeDecls) c ++
            (map ppr . toList . codeStms) c

instance Semigroup Code where
    a <> b = Code
        { codeDefs     = codeDefs a <> codeDefs b
        , codeFunDefs  = codeFunDefs a <> codeFunDefs b
        , codeFunDecls = codeFunDecls a <> codeFunDecls b
        , codeDecls    = codeDecls a <> codeDecls b
        , codeStms     = codeStms a <> codeStms b
        }

instance Monoid Code where
    mempty = Code
        { codeDefs     = mempty
        , codeFunDefs  = mempty
        , codeFunDecls = mempty
        , codeDecls    = mempty
        , codeStms     = mempty
        }

    mappend = (<>)
