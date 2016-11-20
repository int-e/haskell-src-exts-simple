{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module reexports a simplified view on "Language.Haskell.Exts.Syntax".
-- The idea is to expose datatypes like
--
-- > data Name l = Ident l String | Symbol l String
--
-- using ghc's pattern synonyms:
--
-- > type Name = H.Name ()
-- > pattern Ident a = H.Ident () a
-- > pattern Symbol a = H.Symbol () a
--
-- This works nicely for all datatypes with two exception:
--
-- * `ImportDecl` has a record constructor. Record type synonyms are only
--   supported ghc-8.0 and later, so for ghc-7.10 and earlier, the
--   constructor is exported as a plain constructor, and the record fields
--   as function.
-- * `Literal` has constructors with an extra `String` argument that is not
--   used by `Language.Haskell.Exts.Simple.Pretty`. This module uses explicitly
--   bidirectional pattern synonyms to support this type, but support for that
--   is only available in ghc-7.10 and later.
--
-- __IMPORTANT__: if you require compatiblity with ghc 7.8, you should use the
-- functions `charL`, `stringL` etc. for constructing `Literal` values!

module Language.Haskell.Exts.Simple.Syntax (
    module Language.Haskell.Exts.Simple.Syntax,
    module Language.Haskell.Exts.Syntax
) where

import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.Syntax (
    Boxed (..),
    Tool (..),
 )

-- * Datatypes and Constructors

-- ** `H.ModuleName`
type ModuleName = H.ModuleName ()
pattern ModuleName a = H.ModuleName () (a :: String) :: ModuleName

-- ** `H.SpecialCon`
type SpecialCon = H.SpecialCon ()
pattern UnitCon = H.UnitCon () :: SpecialCon
pattern ListCon = H.ListCon () :: SpecialCon
pattern FunCon = H.FunCon () :: SpecialCon
pattern TupleCon a b = H.TupleCon () (a :: Boxed) (b :: Int) :: SpecialCon
pattern Cons = H.Cons () :: SpecialCon
pattern UnboxedSingleCon = H.UnboxedSingleCon () :: SpecialCon

-- ** `H.QName`
type QName = H.QName ()
pattern Qual a b = H.Qual () (a :: ModuleName) (b :: Name) :: QName
pattern UnQual a = H.UnQual () (a :: Name) :: QName
pattern Special a = H.Special () (a :: SpecialCon) :: QName

-- ** `H.Name`
type Name = H.Name ()
pattern Ident a = H.Ident () (a :: String) :: Name
pattern Symbol a = H.Symbol () (a :: String) :: Name

-- ** `H.IPName`
type IPName = H.IPName ()
pattern IPDup a = H.IPDup () (a :: String) :: IPName
pattern IPLin a = H.IPLin () (a :: String) :: IPName

-- ** `H.QOp`
type QOp = H.QOp ()
pattern QVarOp a = H.QVarOp () (a :: QName) :: QOp
pattern QConOp a = H.QConOp () (a :: QName) :: QOp

-- ** `H.Op`
type Op = H.Op ()
pattern VarOp a = H.VarOp () (a :: Name) :: Op
pattern ConOp a = H.ConOp () (a :: Name) :: Op

-- ** `H.CName`
type CName = H.CName ()
pattern VarName a = H.VarName () (a :: Name) :: CName
pattern ConName a = H.ConName () (a :: Name) :: CName

-- ** `H.Module`
type Module = H.Module ()
pattern Module a b c d = H.Module () (a :: (Maybe ModuleHead)) (b :: [ModulePragma]) (c :: [ImportDecl]) (d :: [Decl]) :: Module
pattern XmlPage a b c d e f = H.XmlPage () (a :: ModuleName) (b :: [ModulePragma]) (c :: XName) (d :: [XAttr]) (e :: (Maybe Exp)) (f :: [Exp]) :: Module
pattern XmlHybrid a b c d e f g h = H.XmlHybrid () (a :: (Maybe ModuleHead)) (b :: [ModulePragma]) (c :: [ImportDecl]) (d :: [Decl]) (e :: XName) (f :: [XAttr]) (g :: (Maybe Exp)) (h :: [Exp]) :: Module

-- ** `H.ModuleHead`
type ModuleHead = H.ModuleHead ()
pattern ModuleHead a b c = H.ModuleHead () (a :: ModuleName) (b :: (Maybe WarningText)) (c :: (Maybe ExportSpecList)) :: ModuleHead

-- ** `H.ExportSpecList`
type ExportSpecList = H.ExportSpecList ()
pattern ExportSpecList a = H.ExportSpecList () (a :: [ExportSpec]) :: ExportSpecList

-- ** `H.ExportSpec`
type ExportSpec = H.ExportSpec ()
pattern EVar a = H.EVar () (a :: QName) :: ExportSpec
pattern EAbs a b = H.EAbs () (a :: Namespace) (b :: QName) :: ExportSpec
pattern EThingWith a b c = H.EThingWith () (a :: EWildcard) (b :: QName) (c :: [CName]) :: ExportSpec
pattern EModuleContents a = H.EModuleContents () (a :: ModuleName) :: ExportSpec

-- ** `H.EWildcard`
type EWildcard = H.EWildcard ()
pattern NoWildcard = H.NoWildcard () :: EWildcard
pattern EWildcard a = H.EWildcard () (a :: Int) :: EWildcard

-- ** `H.Namespace`
type Namespace = H.Namespace ()
pattern NoNamespace = H.NoNamespace () :: Namespace
pattern TypeNamespace = H.TypeNamespace () :: Namespace
pattern PatternNamespace = H.PatternNamespace () :: Namespace

-- ** `H.ImportDecl`
type ImportDecl = H.ImportDecl ()
-- | Note, this is originally a record constructor, and we use a pattern record synonym for ghc-8.0. But for earlier ghc versions, `ImportDecl` is a plain pattern synonym, and the selectors are exported as functions.
#if __GLASGOW_HASKELL__ < 800
pattern ImportDecl a b c d e f g = H.ImportDecl () (a :: ModuleName) (b :: Bool) (c :: Bool) (d :: Bool) (e :: Maybe String) (f :: Maybe ModuleName) (g :: Maybe ImportSpecList) :: ImportDecl
importModule :: ImportDecl -> ModuleName
importModule = H.importModule
importQualified :: ImportDecl -> Bool
importQualified = H.importQualified
importSrc :: ImportDecl -> Bool
importSrc = H.importSrc
importSafe :: ImportDecl -> Bool
importSafe = H.importSafe
importPkg :: ImportDecl -> Maybe String
importPkg = H.importPkg
importAs :: ImportDecl -> Maybe ModuleName
importAs = H.importAs
importSpecs :: ImportDecl -> Maybe ImportSpecList
importSpecs = H.importSpecs
#else
pattern ImportDecl { importModule, importQualified, importSrc, importSafe, importPkg, importAs, importSpecs } =
    H.ImportDecl () importModule importQualified importSrc importSafe importPkg importAs importSpecs :: ImportDecl
#endif

-- ** `H.ImportSpecList`
type ImportSpecList = H.ImportSpecList ()
pattern ImportSpecList a b = H.ImportSpecList () (a :: Bool) (b :: [ImportSpec]) :: ImportSpecList

-- ** `H.ImportSpec`
type ImportSpec = H.ImportSpec ()
pattern IVar a = H.IVar () (a :: Name) :: ImportSpec
pattern IAbs a b = H.IAbs () (a :: Namespace) (b :: Name) :: ImportSpec
pattern IThingAll a = H.IThingAll () (a :: Name) :: ImportSpec
pattern IThingWith a b = H.IThingWith () (a :: Name) (b :: [CName]) :: ImportSpec

-- ** `H.Assoc`
type Assoc = H.Assoc ()
pattern AssocNone = H.AssocNone () :: Assoc
pattern AssocLeft = H.AssocLeft () :: Assoc
pattern AssocRight = H.AssocRight () :: Assoc

-- ** `H.Decl`
type Decl = H.Decl ()
pattern TypeDecl a b = H.TypeDecl () (a :: DeclHead) (b :: Type) :: Decl
pattern TypeFamDecl a b c = H.TypeFamDecl () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) :: Decl
pattern ClosedTypeFamDecl a b c d = H.ClosedTypeFamDecl () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) (d :: [TypeEqn]) :: Decl
pattern DataDecl a b c d e = H.DataDecl () (a :: DataOrNew) (b :: (Maybe Context)) (c :: DeclHead) (d :: [QualConDecl]) (e :: (Maybe Deriving)) :: Decl
pattern GDataDecl a b c d e f = H.GDataDecl () (a :: DataOrNew) (b :: (Maybe Context)) (c :: DeclHead) (d :: (Maybe Kind)) (e :: [GadtDecl]) (f :: (Maybe Deriving)) :: Decl
pattern DataFamDecl a b c = H.DataFamDecl () (a :: (Maybe Context)) (b :: DeclHead) (c :: (Maybe ResultSig)) :: Decl
pattern TypeInsDecl a b = H.TypeInsDecl () (a :: Type) (b :: Type) :: Decl
pattern DataInsDecl a b c d = H.DataInsDecl () (a :: DataOrNew) (b :: Type) (c :: [QualConDecl]) (d :: (Maybe Deriving)) :: Decl
pattern GDataInsDecl a b c d e = H.GDataInsDecl () (a :: DataOrNew) (b :: Type) (c :: (Maybe Kind)) (d :: [GadtDecl]) (e :: (Maybe Deriving)) :: Decl
pattern ClassDecl a b c d = H.ClassDecl () (a :: (Maybe Context)) (b :: DeclHead) (c :: [FunDep]) (d :: (Maybe [ClassDecl])) :: Decl
pattern InstDecl a b c = H.InstDecl () (a :: (Maybe Overlap)) (b :: InstRule) (c :: (Maybe [InstDecl])) :: Decl
pattern DerivDecl a b = H.DerivDecl () (a :: (Maybe Overlap)) (b :: InstRule) :: Decl
pattern InfixDecl a b c = H.InfixDecl () (a :: Assoc) (b :: (Maybe Int)) (c :: [Op]) :: Decl
pattern DefaultDecl a = H.DefaultDecl () (a :: [Type]) :: Decl
pattern SpliceDecl a = H.SpliceDecl () (a :: Exp) :: Decl
pattern TypeSig a b = H.TypeSig () (a :: [Name]) (b :: Type) :: Decl
pattern PatSynSig a b c d e = H.PatSynSig () (a :: Name) (b :: (Maybe [TyVarBind])) (c :: (Maybe Context)) (d :: (Maybe Context)) (e :: Type) :: Decl
pattern FunBind a = H.FunBind () (a :: [Match]) :: Decl
pattern PatBind a b c = H.PatBind () (a :: Pat) (b :: Rhs) (c :: (Maybe Binds)) :: Decl
pattern PatSyn a b c = H.PatSyn () (a :: Pat) (b :: Pat) (c :: PatternSynDirection) :: Decl
pattern ForImp a b c d e = H.ForImp () (a :: CallConv) (b :: (Maybe Safety)) (c :: (Maybe String)) (d :: Name) (e :: Type) :: Decl
pattern ForExp a b c d = H.ForExp () (a :: CallConv) (b :: (Maybe String)) (c :: Name) (d :: Type) :: Decl
pattern RulePragmaDecl a = H.RulePragmaDecl () (a :: [Rule]) :: Decl
pattern DeprPragmaDecl a = H.DeprPragmaDecl () (a :: [([Name], String)]) :: Decl
pattern WarnPragmaDecl a = H.WarnPragmaDecl () (a :: [([Name], String)]) :: Decl
pattern InlineSig a b c = H.InlineSig () (a :: Bool) (b :: (Maybe Activation)) (c :: QName) :: Decl
pattern InlineConlikeSig a b = H.InlineConlikeSig () (a :: (Maybe Activation)) (b :: QName) :: Decl
pattern SpecSig a b c = H.SpecSig () (a :: (Maybe Activation)) (b :: QName) (c :: [Type]) :: Decl
pattern SpecInlineSig a b c d = H.SpecInlineSig () (a :: Bool) (b :: (Maybe Activation)) (c :: QName) (d :: [Type]) :: Decl
pattern InstSig a = H.InstSig () (a :: InstRule) :: Decl
pattern AnnPragma a = H.AnnPragma () (a :: Annotation) :: Decl
pattern MinimalPragma a = H.MinimalPragma () (a :: (Maybe BooleanFormula)) :: Decl
pattern RoleAnnotDecl a b = H.RoleAnnotDecl () (a :: QName) (b :: [Role]) :: Decl

-- ** `H.PatternSynDirection`
type PatternSynDirection = H.PatternSynDirection ()
pattern Unidirectional = H.Unidirectional :: PatternSynDirection
pattern ImplicitBidirectional = H.ImplicitBidirectional :: PatternSynDirection
pattern ExplicitBidirectional a = H.ExplicitBidirectional () (a :: [Decl]) :: PatternSynDirection

-- ** `H.TypeEqn`
type TypeEqn = H.TypeEqn ()
pattern TypeEqn a b = H.TypeEqn () (a :: Type) (b :: Type) :: TypeEqn

-- ** `H.Annotation`
type Annotation = H.Annotation ()
pattern Ann a b = H.Ann () (a :: Name) (b :: Exp) :: Annotation
pattern TypeAnn a b = H.TypeAnn () (a :: Name) (b :: Exp) :: Annotation
pattern ModuleAnn a = H.ModuleAnn () (a :: Exp) :: Annotation

-- ** `H.BooleanFormula`
type BooleanFormula = H.BooleanFormula ()
pattern VarFormula a = H.VarFormula () (a :: Name) :: BooleanFormula
pattern AndFormula a = H.AndFormula () (a :: [BooleanFormula]) :: BooleanFormula
pattern OrFormula a = H.OrFormula () (a :: [BooleanFormula]) :: BooleanFormula
pattern ParenFormula a = H.ParenFormula () (a :: BooleanFormula) :: BooleanFormula

-- ** `H.Role`
type Role = H.Role ()
pattern Nominal = H.Nominal () :: Role
pattern Representational = H.Representational () :: Role
pattern Phantom = H.Phantom () :: Role
pattern RoleWildcard = H.RoleWildcard () :: Role

-- ** `H.DataOrNew`
type DataOrNew = H.DataOrNew ()
pattern DataType = H.DataType () :: DataOrNew
pattern NewType = H.NewType () :: DataOrNew

-- ** `H.InjectivityInfo`
type InjectivityInfo = H.InjectivityInfo ()
pattern InjectivityInfo a b = H.InjectivityInfo () (a :: Name) (b :: [Name]) :: InjectivityInfo

-- ** `H.ResultSig`
type ResultSig = H.ResultSig ()
pattern KindSig a = H.KindSig () (a :: Kind) :: ResultSig
pattern TyVarSig a = H.TyVarSig () (a :: TyVarBind) :: ResultSig

-- ** `H.DeclHead`
type DeclHead = H.DeclHead ()
pattern DHead a = H.DHead () (a :: Name) :: DeclHead
pattern DHInfix a b = H.DHInfix () (a :: TyVarBind) (b :: Name) :: DeclHead
pattern DHParen a = H.DHParen () (a :: DeclHead) :: DeclHead
pattern DHApp a b = H.DHApp () (a :: DeclHead) (b :: TyVarBind) :: DeclHead

-- ** `H.InstRule`
type InstRule = H.InstRule ()
pattern IRule a b c = H.IRule () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: InstHead) :: InstRule
pattern IParen a = H.IParen () (a :: InstRule) :: InstRule

-- ** `H.InstHead`
type InstHead = H.InstHead ()
pattern IHCon a = H.IHCon () (a :: QName) :: InstHead
pattern IHInfix a b = H.IHInfix () (a :: Type) (b :: QName) :: InstHead
pattern IHParen a = H.IHParen () (a :: InstHead) :: InstHead
pattern IHApp a b = H.IHApp () (a :: InstHead) (b :: Type) :: InstHead

-- ** `H.Deriving`
type Deriving = H.Deriving ()
pattern Deriving a = H.Deriving () (a :: [InstRule]) :: Deriving

-- ** `H.Binds`
type Binds = H.Binds ()
pattern BDecls a = H.BDecls () (a :: [Decl]) :: Binds
pattern IPBinds a = H.IPBinds () (a :: [IPBind]) :: Binds

-- ** `H.IPBind`
type IPBind = H.IPBind ()
pattern IPBind a b = H.IPBind () (a :: IPName) (b :: Exp) :: IPBind

-- ** `H.Match`
type Match = H.Match ()
pattern Match a b c d = H.Match () (a :: Name) (b :: [Pat]) (c :: Rhs) (d :: (Maybe Binds)) :: Match
pattern InfixMatch a b c d e = H.InfixMatch () (a :: Pat) (b :: Name) (c :: [Pat]) (d :: Rhs) (e :: (Maybe Binds)) :: Match

-- ** `H.QualConDecl`
type QualConDecl = H.QualConDecl ()
pattern QualConDecl a b c = H.QualConDecl () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: ConDecl) :: QualConDecl

-- ** `H.ConDecl`
type ConDecl = H.ConDecl ()
pattern ConDecl a b = H.ConDecl () (a :: Name) (b :: [Type]) :: ConDecl
pattern InfixConDecl a b c = H.InfixConDecl () (a :: Type) (b :: Name) (c :: Type) :: ConDecl
pattern RecDecl a b = H.RecDecl () (a :: Name) (b :: [FieldDecl]) :: ConDecl

-- ** `H.FieldDecl`
type FieldDecl = H.FieldDecl ()
pattern FieldDecl a b = H.FieldDecl () (a :: [Name]) (b :: Type) :: FieldDecl

-- ** `H.GadtDecl`
type GadtDecl = H.GadtDecl ()
pattern GadtDecl a b c = H.GadtDecl () (a :: Name) (b :: (Maybe [FieldDecl])) (c :: Type) :: GadtDecl

-- ** `H.ClassDecl`
type ClassDecl = H.ClassDecl ()
pattern ClsDecl a = H.ClsDecl () (a :: Decl) :: ClassDecl
pattern ClsDataFam a b c = H.ClsDataFam () (a :: (Maybe Context)) (b :: DeclHead) (c :: (Maybe ResultSig)) :: ClassDecl
pattern ClsTyFam a b c = H.ClsTyFam () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) :: ClassDecl
pattern ClsTyDef a = H.ClsTyDef () (a :: TypeEqn) :: ClassDecl
pattern ClsDefSig a b = H.ClsDefSig () (a :: Name) (b :: Type) :: ClassDecl

-- ** `H.InstDecl`
type InstDecl = H.InstDecl ()
pattern InsDecl a = H.InsDecl () (a :: Decl) :: InstDecl
pattern InsType a b = H.InsType () (a :: Type) (b :: Type) :: InstDecl
pattern InsData a b c d = H.InsData () (a :: DataOrNew) (b :: Type) (c :: [QualConDecl]) (d :: (Maybe Deriving)) :: InstDecl
pattern InsGData a b c d e = H.InsGData () (a :: DataOrNew) (b :: Type) (c :: (Maybe Kind)) (d :: [GadtDecl]) (e :: (Maybe Deriving)) :: InstDecl

-- ** `H.BangType`
type BangType = H.BangType ()
pattern BangedTy = H.BangedTy () :: BangType
pattern LazyTy = H.LazyTy () :: BangType
pattern NoStrictAnnot = H.NoStrictAnnot () :: BangType

-- ** `H.Unpackedness`
type Unpackedness = H.Unpackedness ()
pattern Unpack = H.Unpack () :: Unpackedness
pattern NoUnpack = H.NoUnpack () :: Unpackedness
pattern NoUnpackPragma = H.NoUnpackPragma () :: Unpackedness

-- ** `H.Rhs`
type Rhs = H.Rhs ()
pattern UnGuardedRhs a = H.UnGuardedRhs () (a :: Exp) :: Rhs
pattern GuardedRhss a = H.GuardedRhss () (a :: [GuardedRhs]) :: Rhs

-- ** `H.GuardedRhs`
type GuardedRhs = H.GuardedRhs ()
pattern GuardedRhs a b = H.GuardedRhs () (a :: [Stmt]) (b :: Exp) :: GuardedRhs

-- ** `H.Type`
type Type = H.Type ()
pattern TyForall a b c = H.TyForall () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: Type) :: Type
pattern TyFun a b = H.TyFun () (a :: Type) (b :: Type) :: Type
pattern TyTuple a b = H.TyTuple () (a :: Boxed) (b :: [Type]) :: Type
pattern TyList a = H.TyList () (a :: Type) :: Type
pattern TyParArray a = H.TyParArray () (a :: Type) :: Type
pattern TyApp a b = H.TyApp () (a :: Type) (b :: Type) :: Type
pattern TyVar a = H.TyVar () (a :: Name) :: Type
pattern TyCon a = H.TyCon () (a :: QName) :: Type
pattern TyParen a = H.TyParen () (a :: Type) :: Type
pattern TyInfix a b c = H.TyInfix () (a :: Type) (b :: QName) (c :: Type) :: Type
pattern TyKind a b = H.TyKind () (a :: Type) (b :: Kind) :: Type
pattern TyPromoted a = H.TyPromoted () (a :: Promoted) :: Type
pattern TyEquals a b = H.TyEquals () (a :: Type) (b :: Type) :: Type
pattern TySplice a = H.TySplice () (a :: Splice) :: Type
pattern TyBang a b c = H.TyBang () (a :: BangType) (b :: Unpackedness) (c :: Type) :: Type
pattern TyWildCard a = H.TyWildCard () (a :: (Maybe Name)) :: Type
pattern TyQuasiQuote a b = H.TyQuasiQuote () (a :: String) (b :: String) :: Type

-- ** `H.Promoted`
type Promoted = H.Promoted ()
pattern PromotedInteger a b = H.PromotedInteger () (a :: Integer) (b :: String) :: Promoted
pattern PromotedString a b = H.PromotedString () (a :: String) (b :: String) :: Promoted
pattern PromotedCon a b = H.PromotedCon () (a :: Bool) (b :: QName) :: Promoted
pattern PromotedList a b = H.PromotedList () (a :: Bool) (b :: [Type]) :: Promoted
pattern PromotedTuple a = H.PromotedTuple () (a :: [Type]) :: Promoted
pattern PromotedUnit = H.PromotedUnit () :: Promoted

-- skipped: data Boxed

-- ** `H.TyVarBind`
type TyVarBind = H.TyVarBind ()
pattern KindedVar a b = H.KindedVar () (a :: Name) (b :: Kind) :: TyVarBind
pattern UnkindedVar a = H.UnkindedVar () (a :: Name) :: TyVarBind

-- ** `H.Kind`
type Kind = H.Kind ()
pattern KindStar = H.KindStar () :: Kind
pattern KindFn a b = H.KindFn () (a :: Kind) (b :: Kind) :: Kind
pattern KindParen a = H.KindParen () (a :: Kind) :: Kind
pattern KindVar a = H.KindVar () (a :: QName) :: Kind
pattern KindApp a b = H.KindApp () (a :: Kind) (b :: Kind) :: Kind
pattern KindTuple a = H.KindTuple () (a :: [Kind]) :: Kind
pattern KindList a = H.KindList () (a :: Kind) :: Kind

-- ** `H.FunDep`
type FunDep = H.FunDep ()
pattern FunDep a b = H.FunDep () (a :: [Name]) (b :: [Name]) :: FunDep

-- ** `H.Context`
type Context = H.Context ()
pattern CxSingle a = H.CxSingle () (a :: Asst) :: Context
pattern CxTuple a = H.CxTuple () (a :: [Asst]) :: Context
pattern CxEmpty = H.CxEmpty () :: Context

-- ** `H.Asst`
type Asst = H.Asst ()
pattern ClassA a b = H.ClassA () (a :: QName) (b :: [Type]) :: Asst
pattern AppA a b = H.AppA () (a :: Name) (b :: [Type]) :: Asst
pattern InfixA a b c = H.InfixA () (a :: Type) (b :: QName) (c :: Type) :: Asst
pattern IParam a b = H.IParam () (a :: IPName) (b :: Type) :: Asst
pattern EqualP a b = H.EqualP () (a :: Type) (b :: Type) :: Asst
pattern ParenA a = H.ParenA () (a :: Asst) :: Asst
pattern WildCardA a = H.WildCardA () (a :: (Maybe Name)) :: Asst

-- ** `H.Literal`

-- | Beware that the constructors only work in a pattern context in ghc-7.8,
-- because that version does not support explicitly bidirectional pattern
-- synonyms.
--
-- For code that needs to work with ghc-7.8, we provide functions `charL`,
-- `stringL`, `intL`, `fracL`, etc. for constructing `Literal` values.

type Literal = H.Literal ()

#if __GLASGOW_HASKELL__ <= 708
#define where --
#endif

pattern Char a <- H.Char () (a :: Char) _ :: Literal
    where Char a = H.Char () a [a]
charL :: Char -> Literal
charL a = H.Char () a [a]

pattern String a <- H.String () (a :: String) _ :: Literal
    where String a = stringL a
stringL :: String -> Literal
stringL a = H.String () a a

pattern Int a <- H.Int () (a :: Integer) _ :: Literal
    where Int a = intL a
intL :: Integer -> Literal
intL a = H.Int () a (show a)

pattern Frac a <- H.Frac () (a :: Rational) _ :: Literal
    where Frac a = fracL a
fracL :: Rational -> Literal
fracL a = H.Frac () a (show a)

pattern PrimInt a <- H.PrimInt () (a :: Integer) _ :: Literal
    where PrimInt a = primIntL a
primIntL :: Integer -> Literal
primIntL a = H.PrimInt () a (show a)

pattern PrimWord a <- H.PrimWord () (a :: Integer) _ :: Literal
    where PrimWord a = primWordL a
primWordL :: Integer -> Literal
primWordL a = H.PrimWord () a (show a)

pattern PrimFloat a <- H.PrimFloat () (a :: Rational) _ :: Literal
    where PrimFloat a = primFloatL a
primFloatL :: Rational -> Literal
primFloatL a = H.PrimFloat () a (show (fromRational a :: Float))

pattern PrimDouble a <- H.PrimDouble () (a :: Rational) _ :: Literal
    where PrimDouble a = primDoubleL a
primDoubleL :: Rational -> Literal
primDoubleL a = H.PrimDouble () a (show (fromRational a :: Double))

pattern PrimChar a <- H.PrimChar () (a :: Char) _ :: Literal
    where PrimChar a = primCharL a
primCharL :: Char -> Literal
primCharL a = H.PrimChar () a [a]

pattern PrimString a <- H.PrimString () (a :: String) _ :: Literal
    where PrimString a = primStringL a
primStringL :: String -> Literal
primStringL a = H.PrimString () a a

#undef where

-- ** `H.Sign`
type Sign = H.Sign ()
pattern Signless = H.Signless () :: Sign
pattern Negative = H.Negative () :: Sign

-- ** `H.Exp`
type Exp = H.Exp ()
pattern Var a = H.Var () (a :: QName) :: Exp
pattern OverloadedLabel a = H.OverloadedLabel () (a :: String) :: Exp
pattern IPVar a = H.IPVar () (a :: IPName) :: Exp
pattern Con a = H.Con () (a :: QName) :: Exp
pattern Lit a = H.Lit () (a :: Literal) :: Exp
pattern InfixApp a b c = H.InfixApp () (a :: Exp) (b :: QOp) (c :: Exp) :: Exp
pattern App a b = H.App () (a :: Exp) (b :: Exp) :: Exp
pattern NegApp a = H.NegApp () (a :: Exp) :: Exp
pattern Lambda a b = H.Lambda () (a :: [Pat]) (b :: Exp) :: Exp
pattern Let a b = H.Let () (a :: Binds) (b :: Exp) :: Exp
pattern If a b c = H.If () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern MultiIf a = H.MultiIf () (a :: [GuardedRhs]) :: Exp
pattern Case a b = H.Case () (a :: Exp) (b :: [Alt]) :: Exp
pattern Do a = H.Do () (a :: [Stmt]) :: Exp
pattern MDo a = H.MDo () (a :: [Stmt]) :: Exp
pattern Tuple a b = H.Tuple () (a :: Boxed) (b :: [Exp]) :: Exp
pattern TupleSection a b = H.TupleSection () (a :: Boxed) (b :: [Maybe Exp]) :: Exp
pattern List a = H.List () (a :: [Exp]) :: Exp
pattern ParArray a = H.ParArray () (a :: [Exp]) :: Exp
pattern Paren a = H.Paren () (a :: Exp) :: Exp
pattern LeftSection a b = H.LeftSection () (a :: Exp) (b :: QOp) :: Exp
pattern RightSection a b = H.RightSection () (a :: QOp) (b :: Exp) :: Exp
pattern RecConstr a b = H.RecConstr () (a :: QName) (b :: [FieldUpdate]) :: Exp
pattern RecUpdate a b = H.RecUpdate () (a :: Exp) (b :: [FieldUpdate]) :: Exp
pattern EnumFrom a = H.EnumFrom () (a :: Exp) :: Exp
pattern EnumFromTo a b = H.EnumFromTo () (a :: Exp) (b :: Exp) :: Exp
pattern EnumFromThen a b = H.EnumFromThen () (a :: Exp) (b :: Exp) :: Exp
pattern EnumFromThenTo a b c = H.EnumFromThenTo () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern ParArrayFromTo a b = H.ParArrayFromTo () (a :: Exp) (b :: Exp) :: Exp
pattern ParArrayFromThenTo a b c = H.ParArrayFromThenTo () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern ListComp a b = H.ListComp () (a :: Exp) (b :: [QualStmt]) :: Exp
pattern ParComp a b = H.ParComp () (a :: Exp) (b :: [[QualStmt]]) :: Exp
pattern ParArrayComp a b = H.ParArrayComp () (a :: Exp) (b :: [[QualStmt]]) :: Exp
pattern ExpTypeSig a b = H.ExpTypeSig () (a :: Exp) (b :: Type) :: Exp
pattern VarQuote a = H.VarQuote () (a :: QName) :: Exp
pattern TypQuote a = H.TypQuote () (a :: QName) :: Exp
pattern BracketExp a = H.BracketExp () (a :: Bracket) :: Exp
pattern SpliceExp a = H.SpliceExp () (a :: Splice) :: Exp
pattern QuasiQuote a b = H.QuasiQuote () (a :: String) (b :: String) :: Exp
pattern TypeApp a = H.TypeApp () (a :: Type) :: Exp
pattern XTag a b c d = H.XTag () (a :: XName) (b :: [XAttr]) (c :: (Maybe Exp)) (d :: [Exp]) :: Exp
pattern XETag a b c = H.XETag () (a :: XName) (b :: [XAttr]) (c :: (Maybe Exp)) :: Exp
pattern XPcdata a = H.XPcdata () (a :: String) :: Exp
pattern XExpTag a = H.XExpTag () (a :: Exp) :: Exp
pattern XChildTag a = H.XChildTag () (a :: [Exp]) :: Exp
pattern CorePragma a b = H.CorePragma () (a :: String) (b :: Exp) :: Exp
pattern SCCPragma a b = H.SCCPragma () (a :: String) (b :: Exp) :: Exp
pattern GenPragma a b c d = H.GenPragma () (a :: String) (b :: (Int, Int)) (c :: (Int, Int)) (d :: Exp) :: Exp
pattern Proc a b = H.Proc () (a :: Pat) (b :: Exp) :: Exp
pattern LeftArrApp a b = H.LeftArrApp () (a :: Exp) (b :: Exp) :: Exp
pattern RightArrApp a b = H.RightArrApp () (a :: Exp) (b :: Exp) :: Exp
pattern LeftArrHighApp a b = H.LeftArrHighApp () (a :: Exp) (b :: Exp) :: Exp
pattern RightArrHighApp a b = H.RightArrHighApp () (a :: Exp) (b :: Exp) :: Exp
pattern LCase a = H.LCase () (a :: [Alt]) :: Exp
pattern ExprHole = H.ExprHole () :: Exp

-- ** `H.XName`
type XName = H.XName ()
pattern XName a = H.XName () (a :: String) :: XName
pattern XDomName a b = H.XDomName () (a :: String) (b :: String) :: XName

-- ** `H.XAttr`
type XAttr = H.XAttr ()
pattern XAttr a b = H.XAttr () (a :: XName) (b :: Exp) :: XAttr

-- ** `H.Bracket`
type Bracket = H.Bracket ()
pattern ExpBracket a = H.ExpBracket () (a :: Exp) :: Bracket
pattern PatBracket a = H.PatBracket () (a :: Pat) :: Bracket
pattern TypeBracket a = H.TypeBracket () (a :: Type) :: Bracket
pattern DeclBracket a = H.DeclBracket () (a :: [Decl]) :: Bracket

-- ** `H.Splice`
type Splice = H.Splice ()
pattern IdSplice a = H.IdSplice () (a :: String) :: Splice
pattern ParenSplice a = H.ParenSplice () (a :: Exp) :: Splice

-- ** `H.Safety`
type Safety = H.Safety ()
pattern PlayRisky = H.PlayRisky () :: Safety
pattern PlaySafe a = H.PlaySafe () (a :: Bool) :: Safety
pattern PlayInterruptible = H.PlayInterruptible () :: Safety

-- ** `H.CallConv`
type CallConv = H.CallConv ()
pattern StdCall = H.StdCall () :: CallConv
pattern CCall = H.CCall () :: CallConv
pattern CPlusPlus = H.CPlusPlus () :: CallConv
pattern DotNet = H.DotNet () :: CallConv
pattern Jvm = H.Jvm () :: CallConv
pattern Js = H.Js () :: CallConv
pattern JavaScript = H.JavaScript () :: CallConv
pattern CApi = H.CApi () :: CallConv

-- ** `H.ModulePragma`
type ModulePragma = H.ModulePragma ()
pattern LanguagePragma a = H.LanguagePragma () (a :: [Name]) :: ModulePragma
pattern OptionsPragma a b = H.OptionsPragma () (a :: (Maybe Tool)) (b :: String) :: ModulePragma
pattern AnnModulePragma a = H.AnnModulePragma () (a :: Annotation) :: ModulePragma

-- skipped: data Tool

-- ** `H.Overlap`
type Overlap = H.Overlap ()
pattern NoOverlap = H.NoOverlap () :: Overlap
pattern Overlap = H.Overlap () :: Overlap
pattern Incoherent = H.Incoherent () :: Overlap

-- ** `H.Activation`
type Activation = H.Activation ()
pattern ActiveFrom a = H.ActiveFrom () (a :: Int) :: Activation
pattern ActiveUntil a = H.ActiveUntil () (a :: Int) :: Activation

-- ** `H.Rule`
type Rule = H.Rule ()
pattern Rule a b c d e = H.Rule () (a :: String) (b :: (Maybe Activation)) (c :: (Maybe [RuleVar])) (d :: Exp) (e :: Exp) :: Rule

-- ** `H.RuleVar`
type RuleVar = H.RuleVar ()
pattern RuleVar a = H.RuleVar () (a :: Name) :: RuleVar
pattern TypedRuleVar a b = H.TypedRuleVar () (a :: Name) (b :: Type) :: RuleVar

-- ** `H.WarningText`
type WarningText = H.WarningText ()
pattern DeprText a = H.DeprText () (a :: String) :: WarningText
pattern WarnText a = H.WarnText () (a :: String) :: WarningText

-- ** `H.Pat`
type Pat = H.Pat ()
pattern PVar a = H.PVar () (a :: Name) :: Pat
pattern PLit a b = H.PLit () (a :: Sign) (b :: Literal) :: Pat
pattern PNPlusK a b = H.PNPlusK () (a :: Name) (b :: Integer) :: Pat
pattern PInfixApp a b c = H.PInfixApp () (a :: Pat) (b :: QName) (c :: Pat) :: Pat
pattern PApp a b = H.PApp () (a :: QName) (b :: [Pat]) :: Pat
pattern PTuple a b = H.PTuple () (a :: Boxed) (b :: [Pat]) :: Pat
pattern PList a = H.PList () (a :: [Pat]) :: Pat
pattern PParen a = H.PParen () (a :: Pat) :: Pat
pattern PRec a b = H.PRec () (a :: QName) (b :: [PatField]) :: Pat
pattern PAsPat a b = H.PAsPat () (a :: Name) (b :: Pat) :: Pat
pattern PWildCard = H.PWildCard () :: Pat
pattern PIrrPat a = H.PIrrPat () (a :: Pat) :: Pat
pattern PatTypeSig a b = H.PatTypeSig () (a :: Pat) (b :: Type) :: Pat
pattern PViewPat a b = H.PViewPat () (a :: Exp) (b :: Pat) :: Pat
pattern PRPat a = H.PRPat () (a :: [RPat]) :: Pat
pattern PXTag a b c d = H.PXTag () (a :: XName) (b :: [PXAttr]) (c :: (Maybe Pat)) (d :: [Pat]) :: Pat
pattern PXETag a b c = H.PXETag () (a :: XName) (b :: [PXAttr]) (c :: (Maybe Pat)) :: Pat
pattern PXPcdata a = H.PXPcdata () (a :: String) :: Pat
pattern PXPatTag a = H.PXPatTag () (a :: Pat) :: Pat
pattern PXRPats a = H.PXRPats () (a :: [RPat]) :: Pat
pattern PQuasiQuote a b = H.PQuasiQuote () (a :: String) (b :: String) :: Pat
pattern PBangPat a = H.PBangPat () (a :: Pat) :: Pat

-- ** `H.PXAttr`
type PXAttr = H.PXAttr ()
pattern PXAttr a b = H.PXAttr () (a :: XName) (b :: Pat) :: PXAttr

-- ** `H.RPatOp`
type RPatOp = H.RPatOp ()
pattern RPStar = H.RPStar () :: RPatOp
pattern RPStarG = H.RPStarG () :: RPatOp
pattern RPPlus = H.RPPlus () :: RPatOp
pattern RPPlusG = H.RPPlusG () :: RPatOp
pattern RPOpt = H.RPOpt () :: RPatOp
pattern RPOptG = H.RPOptG () :: RPatOp

-- ** `H.RPat`
type RPat = H.RPat ()
pattern RPOp a b = H.RPOp () (a :: RPat) (b :: RPatOp) :: RPat
pattern RPEither a b = H.RPEither () (a :: RPat) (b :: RPat) :: RPat
pattern RPSeq a = H.RPSeq () (a :: [RPat]) :: RPat
pattern RPGuard a b = H.RPGuard () (a :: Pat) (b :: [Stmt]) :: RPat
pattern RPCAs a b = H.RPCAs () (a :: Name) (b :: RPat) :: RPat
pattern RPAs a b = H.RPAs () (a :: Name) (b :: RPat) :: RPat
pattern RPParen a = H.RPParen () (a :: RPat) :: RPat
pattern RPPat a = H.RPPat () (a :: Pat) :: RPat

-- ** `H.PatField`
type PatField = H.PatField ()
pattern PFieldPat a b = H.PFieldPat () (a :: QName) (b :: Pat) :: PatField
pattern PFieldPun a = H.PFieldPun () (a :: QName) :: PatField
pattern PFieldWildcard = H.PFieldWildcard () :: PatField

-- ** `H.Stmt`
type Stmt = H.Stmt ()
pattern Generator a b = H.Generator () (a :: Pat) (b :: Exp) :: Stmt
pattern Qualifier a = H.Qualifier () (a :: Exp) :: Stmt
pattern LetStmt a = H.LetStmt () (a :: Binds) :: Stmt
pattern RecStmt a = H.RecStmt () (a :: [Stmt]) :: Stmt

-- ** `H.QualStmt`
type QualStmt = H.QualStmt ()
pattern QualStmt a = H.QualStmt () (a :: Stmt) :: QualStmt
pattern ThenTrans a = H.ThenTrans () (a :: Exp) :: QualStmt
pattern ThenBy a b = H.ThenBy () (a :: Exp) (b :: Exp) :: QualStmt
pattern GroupBy a = H.GroupBy () (a :: Exp) :: QualStmt
pattern GroupUsing a = H.GroupUsing () (a :: Exp) :: QualStmt
pattern GroupByUsing a b = H.GroupByUsing () (a :: Exp) (b :: Exp) :: QualStmt

-- ** `H.FieldUpdate`
type FieldUpdate = H.FieldUpdate ()
pattern FieldUpdate a b = H.FieldUpdate () (a :: QName) (b :: Exp) :: FieldUpdate
pattern FieldPun a = H.FieldPun () (a :: QName) :: FieldUpdate
pattern FieldWildcard = H.FieldWildcard () :: FieldUpdate

-- ** `H.Alt`
type Alt = H.Alt ()
pattern Alt a b c = H.Alt () (a :: Pat) (b :: Rhs) (c :: (Maybe Binds)) :: Alt

-- * Functions

prelude_mod, main_mod :: ModuleName
prelude_mod = H.prelude_mod ()
main_mod = H.main_mod ()

main_name :: Name
main_name = H.main_name ()

unit_con_name :: QName
unit_con_name = H.unit_con_name ()

tuple_con_name :: Boxed -> Int -> QName
tuple_con_name = H.tuple_con_name ()

list_cons_name :: QName
list_cons_name = H.list_cons_name ()

unboxed_singleton_con_name :: QName
unboxed_singleton_con_name = H.unboxed_singleton_con_name ()

unit_con :: Exp
unit_con = H.unit_con ()

tuple_con :: Boxed -> Int -> Exp
tuple_con = H.tuple_con ()

unboxed_singleton_con :: Exp
unboxed_singleton_con = H.unboxed_singleton_con ()

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: Name
as_name = H.as_name ()
qualified_name = H.qualified_name ()
hiding_name = H.hiding_name ()
minus_name = H.minus_name ()
bang_name = H.bang_name ()
dot_name = H.dot_name ()
star_name = H.star_name ()

export_name, safe_name, unsafe_name, interruptible_name, threadsafe_name, stdcall_name, ccall_name, cplusplus_name, dotnet_name, jvm_name, js_name, javascript_name, capi_name, forall_name, family_name, role_name :: Name
export_name = H.export_name ()
safe_name = H.safe_name ()
unsafe_name = H.unsafe_name ()
interruptible_name = H.interruptible_name ()
threadsafe_name = H.threadsafe_name ()
stdcall_name = H.stdcall_name ()
ccall_name = H.ccall_name ()
cplusplus_name = H.cplusplus_name ()
dotnet_name = H.dotnet_name ()
jvm_name = H.jvm_name ()
js_name = H.js_name ()
javascript_name = H.javascript_name ()
capi_name = H.capi_name ()
forall_name = H.forall_name ()
family_name = H.family_name ()
role_name = H.role_name ()

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: QName
unit_tycon_name = H.unit_tycon_name ()
fun_tycon_name = H.fun_tycon_name ()
list_tycon_name = H.list_tycon_name ()
unboxed_singleton_tycon_name = H.unboxed_singleton_tycon_name ()

tuple_tycon_name :: Boxed -> Int -> QName
tuple_tycon_name = H.tuple_tycon_name ()

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: Type
unit_tycon = H.unit_tycon ()
fun_tycon = H.fun_tycon ()
list_tycon = H.list_tycon ()
unboxed_singleton_tycon = H.unboxed_singleton_tycon ()

tuple_tycon :: Boxed -> Int -> Type
tuple_tycon = H.tuple_tycon ()

-- skipped: (=~=) :: (Annotated a, Eq (a ())) => a l1 -> a l2 -> Bool

-- skipped: class Annotated ast
