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

-- omitted: assocNone, assocLeft, assocRight :: Assoc ()

-- omitted: askFixity :: [Fixity] -> QOp l -> (Assoc (), Int)

-- omitted: askFixityP :: [Fixity] -> QName l -> (Assoc (), Int)

-- omitted: askFix :: [Fixity] -> QName l -> (Assoc (), Int)

-- omitted: prefixMinusFixity :: (Assoc (), Int)

-- omitted: fixity :: Assoc () -> Int -> [String] -> [Fixity]

-- omitted: appFixDecls ::
--              Monad m =>
--              Maybe (ModuleName SrcSpanInfo) ->
--                [Fixity] -> [Decl SrcSpanInfo] -> m [Decl SrcSpanInfo]

-- omitted: getFixities :: Maybe (ModuleName l) -> [Decl l] -> [Fixity]

-- omitted: getFixity :: Maybe (ModuleName l) -> Decl l -> [Fixity]

-- omitted: scrub :: Functor f => f a -> f ()

-- omitted: getBindFixities :: Binds l -> [Fixity]

-- omitted: leafFix ::
--          Monad m => [Fixity] -> Exp SrcSpanInfo -> m (Exp SrcSpanInfo)

-- omitted: leafFixP ::
--           Monad m => [Fixity] -> Pat SrcSpanInfo -> m (Pat SrcSpanInfo)

