{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module partially reexports "Language.Haskell.Exts.Parser" with adaptations.
--
-- __IMPORTANT__: if you require compatiblity with ghc 7.8, you should use the
-- function `listOf` for constructing `ListOf` values!

module Language.Haskell.Exts.Simple.Parser (
    module Language.Haskell.Exts.Simple.Parser,
    module Language.Haskell.Exts.Parser
) where

import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.SrcLoc as H
import Language.Haskell.Exts.Parser (
    Parseable,
    ParseResult (..),
    ParseMode (..),
    defaultParseMode,
    fromParseResult,
    NonGreedy (..),
    unListOf,
 )

import Language.Haskell.Exts.Simple.Syntax

-- * Datatypes and Constructors

-- ** `H.ListOf`
-- | Beware that the `ListOf` constructor only works in a pattern context
-- in ghc-7.8, because that version does not support explicitly bidirectional
-- pattern synonyms.
--
-- For code that needs to work with ghc-7.8, we provide the `listOf` function
-- constructing `ListOf` values.

type ListOf = H.ListOf
pattern ListOf a <- H.ListOf _ a
#if __GLASGOW_HASKELL__ > 708
   where ListOf a = listOf a
#endif
listOf :: [a] -> ListOf a
listOf a = H.ListOf H.noSrcSpan a

-- ** `H.PragmasAndModuleName`
type PragmasAndModuleName = H.PragmasAndModuleName ()
pattern PragmasAndModuleName a b = H.PragmasAndModuleName () (a :: [ModulePragma]) (b :: Maybe ModuleName) :: PragmasAndModuleName

-- ** `H.PragmasAndModuleHead`
type PragmasAndModuleHead = H.PragmasAndModuleHead ()
pattern PragmasAndModuleHead a b = H.PragmasAndModuleHead () (a :: [ModulePragma]) (b :: Maybe ModuleHead)

-- ** `H.ModuleHeadAndImports`
type ModuleHeadAndImports = H.ModuleHeadAndImports ()
pattern ModuleHeadAndImports a b c = H.ModuleHeadAndImports () (a :: [ModulePragma]) (b :: Maybe ModuleHead) (c :: [ImportDecl])

-- * Functions

parse :: (Parseable (ast H.SrcSpanInfo), Functor ast) => String -> ParseResult (ast ())
parse = fmap (fmap (const () :: H.SrcSpanInfo -> ())) . H.parse

parseWithMode :: (Parseable (ast H.SrcSpanInfo), Functor ast) => ParseMode -> String -> ParseResult (ast ())
parseWithMode m = fmap (fmap (const () :: H.SrcSpanInfo -> ())) . H.parseWithMode m

parseModule :: String -> ParseResult Module
parseModule = fmap (fmap (const ())) . H.parseModule

parseModuleWithMode :: ParseMode -> String -> ParseResult Module
parseModuleWithMode m = fmap (fmap (const ())) . H.parseModuleWithMode m

parseExp :: String -> ParseResult Exp
parseExp = fmap (fmap (const ())) . H.parseExp

parseExpWithMode :: ParseMode -> String -> ParseResult Exp
parseExpWithMode m = fmap (fmap (const ())) . H.parseExpWithMode m

parseStmt :: String -> ParseResult Stmt
parseStmt = fmap (fmap (const ())) . H.parseStmt

parseStmtWithMode :: ParseMode -> String -> ParseResult Stmt
parseStmtWithMode m = fmap (fmap (const ())) . H.parseStmtWithMode m

parsePat :: String -> ParseResult Pat
parsePat = fmap (fmap (const ())) . H.parsePat

parsePatWithMode :: ParseMode -> String -> ParseResult Pat
parsePatWithMode m = fmap (fmap (const ())) . H.parsePatWithMode m

parseDecl :: String -> ParseResult Decl
parseDecl = fmap (fmap (const ())) . H.parseDecl

parseDeclWithMode :: ParseMode -> String -> ParseResult Decl
parseDeclWithMode m = fmap (fmap (const ())) . H.parseDeclWithMode m

parseType :: String -> ParseResult Type
parseType = fmap (fmap (const ())) . H.parseType

parseTypeWithMode :: ParseMode -> String -> ParseResult Type
parseTypeWithMode m = fmap (fmap (const ())) . H.parseTypeWithMode m

parseImportDecl :: String -> ParseResult ImportDecl
parseImportDecl = fmap (fmap (const ())) . H.parseImportDecl

parseImportDeclWithMode :: ParseMode -> String -> ParseResult ImportDecl
parseImportDeclWithMode m = fmap (fmap (const ())) . H.parseImportDeclWithMode m

getTopPragmas :: String -> ParseResult [ModulePragma]
getTopPragmas = fmap (fmap (fmap (const ()))) . H.getTopPragmas

-- omitted parsing functions, new in haskell-source-exts-1.20:
--
-- parseModuleWithComments ::
--     ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
--
-- parseExpWithComments ::
--     ParseMode -> String -> ParseResult (Exp SrcSpanInfo, [Comment])
--
-- parsePatWithComments ::
--     ParseMode -> String -> ParseResult (Pat SrcSpanInfo, [Comment])
--
-- parseDeclWithComments ::
--     ParseMode -> String -> ParseResult (Decl SrcSpanInfo, [Comment])
--
-- parseTypeWithComments ::
--     ParseMode -> String -> ParseResult (Type SrcSpanInfo, [Comment])
--
-- parseStmt ::
--     String -> ParseResult (Stmt SrcSpanInfo)
--
-- parseStmtWithMode ::
--     ParseMode -> String -> ParseResult (Stmt SrcSpanInfo)
--
-- parseStmtWithComments ::
--     ParseMode -> String -> ParseResult (Stmt SrcSpanInfo, [Comment])
--
-- parseImportDeclWithComments ::
--     ParseMode -> String -> ParseResult (ImportDecl SrcSpanInfo, [Comment])

-- omitted: data NonGreedy a
