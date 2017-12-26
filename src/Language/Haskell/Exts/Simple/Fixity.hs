-- |
-- This module partially reexports "Language.Haskell.Exts.Fixity", adapting `applyFixityies`.

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
import Control.Monad

-- * Functions

applyFixities :: (AppFixity ast, Functor ast, Monad m) => [Fixity] -> ast () -> m (ast ())
applyFixities fixs = liftM (fmap (const ())) . H.applyFixities fixs . fmap (const noSrcSpan)

-- omitted fixity functionality, new in haskell-source-exts-1.20:
-- assocNone, assocLeft, assocRight :: Assoc ()
--
-- askFixity :: [Fixity] -> QOp l -> (Assoc (), Int)
--
-- askFixityP :: [Fixity] -> QName l -> (Assoc (), Int)
--
-- askFix :: [Fixity] -> QName l -> (Assoc (), Int)
--
-- prefixMinusFixity :: (Assoc (), Int)
--
-- fixity :: Assoc () -> Int -> [String] -> [Fixity]
--
-- appFixDecls :: Monad m =>
--     Maybe (ModuleName SrcSpanInfo) ->
--     [Fixity] -> [Decl SrcSpanInfo] -> m [Decl SrcSpanInfo]
--
-- getFixities :: Maybe (ModuleName l) -> [Decl l] -> [Fixity]
--
-- getFixity :: Maybe (ModuleName l) -> Decl l -> [Fixity]
--
-- scrub :: Functor f => f a -> f ()
--
-- getBindFixities :: Binds l -> [Fixity]
--
-- leafFix :: Monad m =>
--     [Fixity] -> Exp SrcSpanInfo -> m (Exp SrcSpanInfo)
--
-- leafFixP :: Monad m =>
--     [Fixity] -> Pat SrcSpanInfo -> m (Pat SrcSpanInfo)
