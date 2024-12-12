-- Bootstrapping tool:
--
-- Designed to read Language.Haskell.Exts.Syntax
-- and produce a module exporting the corresponding annotation-free AST.
--
-- Note that some manual tweaks to the output will be required.

module Main where

import Language.Haskell.Exts
import System.Environment
import Data.List
import Data.Generics

main = do
    x <- parseModule <$> getContents
    case x of
        ParseOk mod -> mapM_ putStrLn $ module_ mod
        ParseFailed l s -> putStrLn (prettyPrint l ++ ": " ++ s)

module_ (Module _ (Just head) _ _ decl) =
    head_ head ++ (decl >>= decl_)

head_ (ModuleHead _ (ModuleName _ n) _ _) =
    ["{-# LANGUAGE PatternSynonyms #-}",
     "module " ++ n' ++ " (",
     "    module " ++ n' ++ ",",
     "    module " ++ n,
     ") where",
     "",
     "import qualified " ++ n ++ " as H",
     "import " ++ n ++ " (",
     " )",
     ""]
  where
    n' = "Language.Haskell.Exts.Simple" ++ drop 21 n

decl_ (DataDecl _ _ _ (DHApp _ (DHead _ (Ident _ n)) (UnkindedVar _ (Ident _ "l"))) dcons _) =
    ["-- ** `H." ++ n ++ "`",
     "type " ++ n ++ " = H." ++ n ++ " ()"] ++
    (dcons >>= qualConDecl_ n) ++
    [""] ++
    ["#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)"] ++
    ("{-# COMPLETE " : repeat "             ")
      `zipStrs` chunkedConstructors
      `zipStrs` (replicate (length chunkedConstructors - 1) "," ++ [" #-}"]) ++
    ["#endif"] ++
    [""]
  where
    zipStrs = zipWith (++)

    chunkedConstructors
      = map (intercalate ", ")
      $ splitWithMaxLen 75
      $ map getConstructor
      $ dcons

    getConstructor (QualConDecl _ _ _ decl) = case decl of
      ConDecl _ (Ident _ n') _ -> n'
      RecDecl _ (Ident _ n') _ -> n'
      _ -> error "Unexpected ConDecl constructor"

    splitWithMaxLen maxLen = reverse . foldl' f []
      where
        f [] word = [[word]]
        f (strs:strss) word
          | sum (map ((+ 2) . length) (word : strs)) <= maxLen = (strs ++ [word]) : strss
          | otherwise = [word] : strs : strss

decl_ (DataDecl _ _ _ dhead dcons _) =
    ["-- skipped: data " ++ prettyPrint dhead,
     ""]
decl_ (TypeDecl _ dhead _) =
    ["-- skipped: type " ++ prettyPrint dhead,
     ""]
decl_ (TypeSig _ ns (TyFun _ (TyVar _ (Ident _ "l")) t)) =
    [intercalate ", " ns' ++ " :: " ++ prettyPrint t'] ++
    [n ++ " = H." ++ n ++ " ()" | n <- ns'] ++
    [""]
  where
    ns' = [n | Ident _ n <- ns]
    t' = adjType t
decl_ t@TypeSig{} =
    ["-- skipped: " ++ prettyPrint t, ""]
decl_ FunBind{} = []
decl_ PatBind{} = []
decl_ (ClassDecl _ _ head _ _) =
    ["-- skipped: class " ++ prettyPrint head, ""]
decl_ InstDecl{} = []
decl_ e = error $ prettyPrint e

qualConDecl_ t' (QualConDecl _ _ _ (ConDecl _ (Ident _ n) (TyVar _ (Ident _ "l") : ts))) =
    ["pattern " ++ n ++ vars ts ++ " = H." ++ n ++ " ()" ++ vars' ts ++ " :: " ++ t']
qualConDecl_ t' (QualConDecl _ _ _ (ConDecl _ (Ident _ n) ts)) =
    ["pattern " ++ n ++ vars ts ++ " = H." ++ n ++ vars' ts ++ " :: " ++ t']
qualConDecl_ t' (QualConDecl _ _ _ (RecDecl _ (Ident _ n) (FieldDecl _ _ (TyVar _ (Ident _ "l")) : fs))) =
    ["-- TODO: record constructor",
     "pattern " ++ n ++ vars ts ++ " = H." ++ n ++ " ()" ++ vars' ts ++ " :: " ++ t']
  where
    ts = [t | FieldDecl _ _ t <- fs]

vars ts = unwords ("" : (take (length ts) $ map return ['a'..]))
vars' ts = unwords ("" : zipWith (\n t -> "(" ++ [n] ++ " :: " ++ prettyPrint (adjType t) ++ ")") ['a'..] ts)

adjType = everywhere (id `extT` f) where
    f :: Type SrcSpanInfo -> Type SrcSpanInfo
    f (TyApp _ t@(TyCon _ (UnQual _ n)) (TyVar _ (Ident _ "l"))) = t
    f (TyParen _ t@TyCon{}) = t
    f tv  = tv
