-- |
-- This module reexports "Language.Haskell.Exts.Fixity", adapting `applyFixityies`.

module Language.Haskell.Exts.Simple.Fixity (
    module Language.Haskell.Exts.Simple.Fixity,
    module Language.Haskell.Exts.Fixity
) where

import qualified Language.Haskell.Exts.Fixity as H
import Language.Haskell.Exts.Fixity (
    Fixity(..),
    infix_, infixl_, infixr_,
    preludeFixities, baseFixities,
    AppFixity,
 )
import Language.Haskell.Exts.Simple.Syntax
import Language.Haskell.Exts.SrcLoc

-- * Functions

applyFixities :: (AppFixity ast, Functor ast, Monad m) => [Fixity] -> ast () -> m (ast ())
applyFixities fixs = fmap (fmap (const ())) . H.applyFixities fixs . fmap (const noSrcSpan)
