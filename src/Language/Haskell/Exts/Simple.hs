{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Exts.Simple (
    module Language.Haskell.Exts.Simple.Syntax,
    module Language.Haskell.Exts.Simple.Build,
    module Language.Haskell.Exts.Simple.Pretty,
    module Language.Haskell.Exts.Simple.Fixity,
    module Language.Haskell.Exts.Simple.Comments,
    module Language.Haskell.Exts.Simple.Extension,
    module Language.Haskell.Exts.Simple.Parser,
    module Language.Haskell.Exts.Simple
) where

import qualified Language.Haskell.Exts as H
import Language.Haskell.Exts (
    readExtensions,
 )

import Language.Haskell.Exts.Simple.Syntax
import Language.Haskell.Exts.Simple.Build
import Language.Haskell.Exts.Simple.Pretty
import Language.Haskell.Exts.Simple.Fixity
import Language.Haskell.Exts.Simple.Extension
import Language.Haskell.Exts.Simple.Comments
import Language.Haskell.Exts.Simple.Parser

parseFile :: FilePath -> IO (ParseResult Module)
parseFile = fmap (fmap (fmap (const ()))) . H.parseFile

parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts e = fmap (fmap (fmap (const ()))) . H.parseFileWithExts e


parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult Module)
parseFileWithMode m = fmap (fmap (fmap (const ()))) . H.parseFileWithMode m


parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module, [Comment]))
parseFileWithComments m =
    fmap (fmap (\(a, b) -> (fmap (const ()) a, b))) . H.parseFileWithComments m

parseFileWithCommentsAndPragmas :: ParseMode -> FilePath ->IO (ParseResult (Module, [Comment], [UnknownPragma]))
parseFileWithCommentsAndPragmas m =
    fmap (fmap (\(a, b, c) -> (fmap (const ()) a, b, c))) . H.parseFileWithCommentsAndPragmas m

parseFileContents :: String -> ParseResult Module
parseFileContents = fmap (fmap (const ())) . H.parseFileContents

parseFileContentsWithExts :: [Extension] -> String -> ParseResult Module
parseFileContentsWithExts e =
    fmap (fmap (const ())) . H.parseFileContentsWithExts e

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode m =
    fmap (fmap (const ())) . H.parseFileContentsWithMode m

parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])
parseFileContentsWithComments m =
    fmap (\(a, b) -> (fmap (const ()) a, b)) . H.parseFileContentsWithComments m
