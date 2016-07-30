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
    ["type " ++ n ++ " = H." ++ n ++ " ()"] ++
    (dcons >>= qualConDecl_) ++
    [""]
decl_ (DataDecl _ _ _ dhead dcons _) =
    ["-- skipped: data " ++ prettyPrint dhead,
     ""]
decl_ (TypeSig _ ns (TyFun _ (TyVar _ (Ident _ "l")) t)) =
    [intercalate ", " ns' ++ " :: " ++ prettyPrint t'] ++
    [n ++ " = H." ++ n ++ " ()" | n <- ns'] ++
    [""]
  where
    ns' = [n | Ident _ n <- ns]
    f :: Type SrcSpanInfo -> Type SrcSpanInfo
    f (TyApp i t@(TyCon _ (UnQual _ n)) (TyVar _ (Ident _ "l"))) = t
    f tv  = tv
    t' = everywhere (id `extT` f) t
decl_ t@TypeSig{} =
    ["-- skipped: " ++ prettyPrint t, ""]
decl_ FunBind{} = []
decl_ PatBind{} = []
decl_ (ClassDecl _ _ head _ _) =
    ["-- skipped: class " ++ prettyPrint head, ""]
decl_ InstDecl{} = []
decl_ e = error $ prettyPrint e


qualConDecl_ (QualConDecl _ _ _ (ConDecl _ (Ident _ n) ts@(TyVar _ (Ident _ "l") : _))) =
    ["pattern " ++ n ++ vs ++ " = H." ++ n ++ " ()" ++ vs]
  where
    vs = unwords (take (length ts) $ "" : map return ['a'..])
qualConDecl_ (QualConDecl _ _ _ (ConDecl _ (Ident _ n) ts)) =
    ["pattern " ++ n ++ vs ++ " = H." ++ n ++ vs]
  where
    vs = " " ++ unwords (take (length ts) $ map return ['a'..])
qualConDecl_ (QualConDecl _ _ _ (RecDecl _ (Ident _ n) ts@(FieldDecl _ _ (TyVar _ (Ident _ "l")) : _))) =
    ["pattern " ++ n ++ vs ++ " = H." ++ n ++ " ()" ++ vs]
  where
    vs = " " ++ unwords (take (length ts - 1) $ map return ['a'..])
