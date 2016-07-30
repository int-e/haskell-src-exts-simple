{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Exts.Simple.Syntax (
    module Language.Haskell.Exts.Simple.Syntax,
    module Language.Haskell.Exts.Syntax
) where

import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.Syntax (
    Boxed (..),
    Tool (..),
 )

type ModuleName = H.ModuleName ()
pattern ModuleName a = H.ModuleName () a

type SpecialCon = H.SpecialCon ()
pattern UnitCon = H.UnitCon ()
pattern ListCon = H.ListCon ()
pattern FunCon = H.FunCon ()
pattern TupleCon a b = H.TupleCon () a b
pattern Cons = H.Cons ()
pattern UnboxedSingleCon = H.UnboxedSingleCon ()

type QName = H.QName ()
pattern Qual a b = H.Qual () a b
pattern UnQual a = H.UnQual () a
pattern Special a = H.Special () a

type Name = H.Name ()
pattern Ident a = H.Ident () a
pattern Symbol a = H.Symbol () a

type IPName = H.IPName ()
pattern IPDup a = H.IPDup () a
pattern IPLin a = H.IPLin () a

type QOp = H.QOp ()
pattern QVarOp a = H.QVarOp () a
pattern QConOp a = H.QConOp () a

type Op = H.Op ()
pattern VarOp a = H.VarOp () a
pattern ConOp a = H.ConOp () a

type CName = H.CName ()
pattern VarName a = H.VarName () a
pattern ConName a = H.ConName () a

type Module = H.Module ()
pattern Module a b c d = H.Module () a b c d
pattern XmlPage a b c d e f = H.XmlPage () a b c d e f
pattern XmlHybrid a b c d e f g h = H.XmlHybrid () a b c d e f g h

type ModuleHead = H.ModuleHead ()
pattern ModuleHead a b c = H.ModuleHead () a b c

type ExportSpecList = H.ExportSpecList ()
pattern ExportSpecList a = H.ExportSpecList () a

type ExportSpec = H.ExportSpec ()
pattern EVar a = H.EVar () a
pattern EAbs a b = H.EAbs () a b
pattern EThingWith a b c = H.EThingWith () a b c
pattern EModuleContents a = H.EModuleContents () a

type EWildcard = H.EWildcard ()
pattern NoWildcard = H.NoWildcard ()
pattern EWildcard a = H.EWildcard () a

type Namespace = H.Namespace ()
pattern NoNamespace = H.NoNamespace ()
pattern TypeNamespace = H.TypeNamespace ()
pattern PatternNamespace = H.PatternNamespace ()

type ImportDecl = H.ImportDecl ()
pattern ImportDecl a b c d e f g = H.ImportDecl () a b c d e f g

type ImportSpecList = H.ImportSpecList ()
pattern ImportSpecList a b = H.ImportSpecList () a b

type ImportSpec = H.ImportSpec ()
pattern IVar a = H.IVar () a
pattern IAbs a b = H.IAbs () a b
pattern IThingAll a = H.IThingAll () a
pattern IThingWith a b = H.IThingWith () a b

type Assoc = H.Assoc ()
pattern AssocNone = H.AssocNone ()
pattern AssocLeft = H.AssocLeft ()
pattern AssocRight = H.AssocRight ()

type Decl = H.Decl ()
pattern TypeDecl a b = H.TypeDecl () a b
pattern TypeFamDecl a b c = H.TypeFamDecl () a b c
pattern ClosedTypeFamDecl a b c d = H.ClosedTypeFamDecl () a b c d
pattern DataDecl a b c d e = H.DataDecl () a b c d e
pattern GDataDecl a b c d e f = H.GDataDecl () a b c d e f
pattern DataFamDecl a b c = H.DataFamDecl () a b c
pattern TypeInsDecl a b = H.TypeInsDecl () a b
pattern DataInsDecl a b c d = H.DataInsDecl () a b c d
pattern GDataInsDecl a b c d e = H.GDataInsDecl () a b c d e
pattern ClassDecl a b c d = H.ClassDecl () a b c d
pattern InstDecl a b c = H.InstDecl () a b c
pattern DerivDecl a b = H.DerivDecl () a b
pattern InfixDecl a b c = H.InfixDecl () a b c
pattern DefaultDecl a = H.DefaultDecl () a
pattern SpliceDecl a = H.SpliceDecl () a
pattern TypeSig a b = H.TypeSig () a b
pattern PatSynSig a b c d e = H.PatSynSig () a b c d e
pattern FunBind a = H.FunBind () a
pattern PatBind a b c = H.PatBind () a b c
pattern PatSyn a b c = H.PatSyn () a b c
pattern ForImp a b c d e = H.ForImp () a b c d e
pattern ForExp a b c d = H.ForExp () a b c d
pattern RulePragmaDecl a = H.RulePragmaDecl () a
pattern DeprPragmaDecl a = H.DeprPragmaDecl () a
pattern WarnPragmaDecl a = H.WarnPragmaDecl () a
pattern InlineSig a b c = H.InlineSig () a b c
pattern InlineConlikeSig a b = H.InlineConlikeSig () a b
pattern SpecSig a b c = H.SpecSig () a b c
pattern SpecInlineSig a b c d = H.SpecInlineSig () a b c d
pattern InstSig a = H.InstSig () a
pattern AnnPragma a = H.AnnPragma () a
pattern MinimalPragma a = H.MinimalPragma () a
pattern RoleAnnotDecl a b = H.RoleAnnotDecl () a b

type PatternSynDirection = H.PatternSynDirection ()
pattern Unidirectional  = H.Unidirectional 
pattern ImplicitBidirectional  = H.ImplicitBidirectional 
pattern ExplicitBidirectional a = H.ExplicitBidirectional () a

type TypeEqn = H.TypeEqn ()
pattern TypeEqn a b = H.TypeEqn () a b

type Annotation = H.Annotation ()
pattern Ann a b = H.Ann () a b
pattern TypeAnn a b = H.TypeAnn () a b
pattern ModuleAnn a = H.ModuleAnn () a

type BooleanFormula = H.BooleanFormula ()
pattern VarFormula a = H.VarFormula () a
pattern AndFormula a = H.AndFormula () a
pattern OrFormula a = H.OrFormula () a
pattern ParenFormula a = H.ParenFormula () a

type Role = H.Role ()
pattern Nominal = H.Nominal ()
pattern Representational = H.Representational ()
pattern Phantom = H.Phantom ()
pattern RoleWildcard = H.RoleWildcard ()

type DataOrNew = H.DataOrNew ()
pattern DataType = H.DataType ()
pattern NewType = H.NewType ()

type InjectivityInfo = H.InjectivityInfo ()
pattern InjectivityInfo a b = H.InjectivityInfo () a b

type ResultSig = H.ResultSig ()
pattern KindSig a = H.KindSig () a
pattern TyVarSig a = H.TyVarSig () a

type DeclHead = H.DeclHead ()
pattern DHead a = H.DHead () a
pattern DHInfix a b = H.DHInfix () a b
pattern DHParen a = H.DHParen () a
pattern DHApp a b = H.DHApp () a b

type InstRule = H.InstRule ()
pattern IRule a b c = H.IRule () a b c
pattern IParen a = H.IParen () a

type InstHead = H.InstHead ()
pattern IHCon a = H.IHCon () a
pattern IHInfix a b = H.IHInfix () a b
pattern IHParen a = H.IHParen () a
pattern IHApp a b = H.IHApp () a b

type Deriving = H.Deriving ()
pattern Deriving a = H.Deriving () a

type Binds = H.Binds ()
pattern BDecls a = H.BDecls () a
pattern IPBinds a = H.IPBinds () a

type IPBind = H.IPBind ()
pattern IPBind a b = H.IPBind () a b

type Match = H.Match ()
pattern Match a b c d = H.Match () a b c d
pattern InfixMatch a b c d e = H.InfixMatch () a b c d e

type QualConDecl = H.QualConDecl ()
pattern QualConDecl a b c = H.QualConDecl () a b c

type ConDecl = H.ConDecl ()
pattern ConDecl a b = H.ConDecl () a b
pattern InfixConDecl a b c = H.InfixConDecl () a b c
pattern RecDecl a b = H.RecDecl () a b

type FieldDecl = H.FieldDecl ()
pattern FieldDecl a b = H.FieldDecl () a b

type GadtDecl = H.GadtDecl ()
pattern GadtDecl a b c = H.GadtDecl () a b c

type ClassDecl = H.ClassDecl ()
pattern ClsDecl a = H.ClsDecl () a
pattern ClsDataFam a b c = H.ClsDataFam () a b c
pattern ClsTyFam a b c = H.ClsTyFam () a b c
pattern ClsTyDef a = H.ClsTyDef () a
pattern ClsDefSig a b = H.ClsDefSig () a b

type InstDecl = H.InstDecl ()
pattern InsDecl a = H.InsDecl () a
pattern InsType a b = H.InsType () a b
pattern InsData a b c d = H.InsData () a b c d
pattern InsGData a b c d e = H.InsGData () a b c d e

type BangType = H.BangType ()
pattern BangedTy = H.BangedTy ()
pattern LazyTy = H.LazyTy ()
pattern NoStrictAnnot = H.NoStrictAnnot ()

type Unpackedness = H.Unpackedness ()
pattern Unpack = H.Unpack ()
pattern NoUnpack = H.NoUnpack ()
pattern NoUnpackPragma = H.NoUnpackPragma ()

type Rhs = H.Rhs ()
pattern UnGuardedRhs a = H.UnGuardedRhs () a
pattern GuardedRhss a = H.GuardedRhss () a

type GuardedRhs = H.GuardedRhs ()
pattern GuardedRhs a b = H.GuardedRhs () a b

type Type = H.Type ()
pattern TyForall a b c = H.TyForall () a b c
pattern TyFun a b = H.TyFun () a b
pattern TyTuple a b = H.TyTuple () a b
pattern TyList a = H.TyList () a
pattern TyParArray a = H.TyParArray () a
pattern TyApp a b = H.TyApp () a b
pattern TyVar a = H.TyVar () a
pattern TyCon a = H.TyCon () a
pattern TyParen a = H.TyParen () a
pattern TyInfix a b c = H.TyInfix () a b c
pattern TyKind a b = H.TyKind () a b
pattern TyPromoted a = H.TyPromoted () a
pattern TyEquals a b = H.TyEquals () a b
pattern TySplice a = H.TySplice () a
pattern TyBang a b c = H.TyBang () a b c
pattern TyWildCard a = H.TyWildCard () a
pattern TyQuasiQuote a b = H.TyQuasiQuote () a b

type Promoted = H.Promoted ()
pattern PromotedInteger a b = H.PromotedInteger () a b
pattern PromotedString a b = H.PromotedString () a b
pattern PromotedCon a b = H.PromotedCon () a b
pattern PromotedList a b = H.PromotedList () a b
pattern PromotedTuple a = H.PromotedTuple () a
pattern PromotedUnit = H.PromotedUnit ()

type TyVarBind = H.TyVarBind ()
pattern KindedVar a b = H.KindedVar () a b
pattern UnkindedVar a = H.UnkindedVar () a

type Kind = H.Kind ()
pattern KindStar = H.KindStar ()
pattern KindFn a b = H.KindFn () a b
pattern KindParen a = H.KindParen () a
pattern KindVar a = H.KindVar () a
pattern KindApp a b = H.KindApp () a b
pattern KindTuple a = H.KindTuple () a
pattern KindList a = H.KindList () a

type FunDep = H.FunDep ()
pattern FunDep a b = H.FunDep () a b

type Context = H.Context ()
pattern CxSingle a = H.CxSingle () a
pattern CxTuple a = H.CxTuple () a
pattern CxEmpty = H.CxEmpty ()

type Asst = H.Asst ()
pattern ClassA a b = H.ClassA () a b
pattern AppA a b = H.AppA () a b
pattern InfixA a b c = H.InfixA () a b c
pattern IParam a b = H.IParam () a b
pattern EqualP a b = H.EqualP () a b
pattern ParenA a = H.ParenA () a
pattern WildCardA a = H.WildCardA () a

-- literals are extra redundant!
type Literal = H.Literal ()
pattern Char a <- H.Char () a _
    where Char a = H.Char () a [a]
pattern String a <- H.String () a _
    where String a = H.String () a a
pattern Int a <- H.Int () a _
    where Int a = H.Int () a (show a)
pattern Frac a <- H.Frac () a _
    where Frac a = H.Frac () a (show a)
pattern PrimInt a <- H.PrimInt () a _
    where PrimInt a = H.PrimInt () a (show a)
pattern PrimWord a <- H.PrimWord () a _
    where PrimWord a = H.PrimWord () a (show a)
pattern PrimFloat a <- H.PrimFloat () a _
    where PrimFloat a = H.PrimFloat () a (show a)
pattern PrimDouble a <- H.PrimDouble () a _
    where PrimDouble a = H.PrimDouble () a (show a)
pattern PrimChar a <- H.PrimChar () a _
    where PrimChar a = H.PrimChar () a (show a)
pattern PrimString a <- H.PrimString () a _
    where PrimString a = H.PrimString () a (show a)

type Sign = H.Sign ()
pattern Signless = H.Signless ()
pattern Negative = H.Negative ()

type Exp = H.Exp ()
pattern Var a = H.Var () a
pattern OverloadedLabel a = H.OverloadedLabel () a
pattern IPVar a = H.IPVar () a
pattern Con a = H.Con () a
pattern Lit a = H.Lit () a
pattern InfixApp a b c = H.InfixApp () a b c
pattern App a b = H.App () a b
pattern NegApp a = H.NegApp () a
pattern Lambda a b = H.Lambda () a b
pattern Let a b = H.Let () a b
pattern If a b c = H.If () a b c
pattern MultiIf a = H.MultiIf () a
pattern Case a b = H.Case () a b
pattern Do a = H.Do () a
pattern MDo a = H.MDo () a
pattern Tuple a b = H.Tuple () a b
pattern TupleSection a b = H.TupleSection () a b
pattern List a = H.List () a
pattern ParArray a = H.ParArray () a
pattern Paren a = H.Paren () a
pattern LeftSection a b = H.LeftSection () a b
pattern RightSection a b = H.RightSection () a b
pattern RecConstr a b = H.RecConstr () a b
pattern RecUpdate a b = H.RecUpdate () a b
pattern EnumFrom a = H.EnumFrom () a
pattern EnumFromTo a b = H.EnumFromTo () a b
pattern EnumFromThen a b = H.EnumFromThen () a b
pattern EnumFromThenTo a b c = H.EnumFromThenTo () a b c
pattern ParArrayFromTo a b = H.ParArrayFromTo () a b
pattern ParArrayFromThenTo a b c = H.ParArrayFromThenTo () a b c
pattern ListComp a b = H.ListComp () a b
pattern ParComp a b = H.ParComp () a b
pattern ParArrayComp a b = H.ParArrayComp () a b
pattern ExpTypeSig a b = H.ExpTypeSig () a b
pattern VarQuote a = H.VarQuote () a
pattern TypQuote a = H.TypQuote () a
pattern BracketExp a = H.BracketExp () a
pattern SpliceExp a = H.SpliceExp () a
pattern QuasiQuote a b = H.QuasiQuote () a b
pattern TypeApp a = H.TypeApp () a
pattern XTag a b c d = H.XTag () a b c d
pattern XETag a b c = H.XETag () a b c
pattern XPcdata a = H.XPcdata () a
pattern XExpTag a = H.XExpTag () a
pattern XChildTag a = H.XChildTag () a
pattern CorePragma a b = H.CorePragma () a b
pattern SCCPragma a b = H.SCCPragma () a b
pattern GenPragma a b c d = H.GenPragma () a b c d
pattern Proc a b = H.Proc () a b
pattern LeftArrApp a b = H.LeftArrApp () a b
pattern RightArrApp a b = H.RightArrApp () a b
pattern LeftArrHighApp a b = H.LeftArrHighApp () a b
pattern RightArrHighApp a b = H.RightArrHighApp () a b
pattern LCase a = H.LCase () a
pattern ExprHole = H.ExprHole ()

type XName = H.XName ()
pattern XName a = H.XName () a
pattern XDomName a b = H.XDomName () a b

type XAttr = H.XAttr ()
pattern XAttr a b = H.XAttr () a b

type Bracket = H.Bracket ()
pattern ExpBracket a = H.ExpBracket () a
pattern PatBracket a = H.PatBracket () a
pattern TypeBracket a = H.TypeBracket () a
pattern DeclBracket a = H.DeclBracket () a

type Splice = H.Splice ()
pattern IdSplice a = H.IdSplice () a
pattern ParenSplice a = H.ParenSplice () a

type Safety = H.Safety ()
pattern PlayRisky = H.PlayRisky ()
pattern PlaySafe a = H.PlaySafe () a
pattern PlayInterruptible = H.PlayInterruptible ()

type CallConv = H.CallConv ()
pattern StdCall = H.StdCall ()
pattern CCall = H.CCall ()
pattern CPlusPlus = H.CPlusPlus ()
pattern DotNet = H.DotNet ()
pattern Jvm = H.Jvm ()
pattern Js = H.Js ()
pattern JavaScript = H.JavaScript ()
pattern CApi = H.CApi ()

type ModulePragma = H.ModulePragma ()
pattern LanguagePragma a = H.LanguagePragma () a
pattern OptionsPragma a b = H.OptionsPragma () a b
pattern AnnModulePragma a = H.AnnModulePragma () a

type Overlap = H.Overlap ()
pattern NoOverlap = H.NoOverlap ()
pattern Overlap = H.Overlap ()
pattern Incoherent = H.Incoherent ()

type Activation = H.Activation ()
pattern ActiveFrom a = H.ActiveFrom () a
pattern ActiveUntil a = H.ActiveUntil () a

type Rule = H.Rule ()
pattern Rule a b c d e = H.Rule () a b c d e

type RuleVar = H.RuleVar ()
pattern RuleVar a = H.RuleVar () a
pattern TypedRuleVar a b = H.TypedRuleVar () a b

type WarningText = H.WarningText ()
pattern DeprText a = H.DeprText () a
pattern WarnText a = H.WarnText () a

type Pat = H.Pat ()
pattern PVar a = H.PVar () a
pattern PLit a b = H.PLit () a b
pattern PNPlusK a b = H.PNPlusK () a b
pattern PInfixApp a b c = H.PInfixApp () a b c
pattern PApp a b = H.PApp () a b
pattern PTuple a b = H.PTuple () a b
pattern PList a = H.PList () a
pattern PParen a = H.PParen () a
pattern PRec a b = H.PRec () a b
pattern PAsPat a b = H.PAsPat () a b
pattern PWildCard = H.PWildCard ()
pattern PIrrPat a = H.PIrrPat () a
pattern PatTypeSig a b = H.PatTypeSig () a b
pattern PViewPat a b = H.PViewPat () a b
pattern PRPat a = H.PRPat () a
pattern PXTag a b c d = H.PXTag () a b c d
pattern PXETag a b c = H.PXETag () a b c
pattern PXPcdata a = H.PXPcdata () a
pattern PXPatTag a = H.PXPatTag () a
pattern PXRPats a = H.PXRPats () a
pattern PQuasiQuote a b = H.PQuasiQuote () a b
pattern PBangPat a = H.PBangPat () a

type PXAttr = H.PXAttr ()
pattern PXAttr a b = H.PXAttr () a b

type RPatOp = H.RPatOp ()
pattern RPStar = H.RPStar ()
pattern RPStarG = H.RPStarG ()
pattern RPPlus = H.RPPlus ()
pattern RPPlusG = H.RPPlusG ()
pattern RPOpt = H.RPOpt ()
pattern RPOptG = H.RPOptG ()

type RPat = H.RPat ()
pattern RPOp a b = H.RPOp () a b
pattern RPEither a b = H.RPEither () a b
pattern RPSeq a = H.RPSeq () a
pattern RPGuard a b = H.RPGuard () a b
pattern RPCAs a b = H.RPCAs () a b
pattern RPAs a b = H.RPAs () a b
pattern RPParen a = H.RPParen () a
pattern RPPat a = H.RPPat () a

type PatField = H.PatField ()
pattern PFieldPat a b = H.PFieldPat () a b
pattern PFieldPun a = H.PFieldPun () a
pattern PFieldWildcard = H.PFieldWildcard ()

type Stmt = H.Stmt ()
pattern Generator a b = H.Generator () a b
pattern Qualifier a = H.Qualifier () a
pattern LetStmt a = H.LetStmt () a
pattern RecStmt a = H.RecStmt () a

type QualStmt = H.QualStmt ()
pattern QualStmt a = H.QualStmt () a
pattern ThenTrans a = H.ThenTrans () a
pattern ThenBy a b = H.ThenBy () a b
pattern GroupBy a = H.GroupBy () a
pattern GroupUsing a = H.GroupUsing () a
pattern GroupByUsing a b = H.GroupByUsing () a b

type FieldUpdate = H.FieldUpdate ()
pattern FieldUpdate a b = H.FieldUpdate () a b
pattern FieldPun a = H.FieldPun () a
pattern FieldWildcard = H.FieldWildcard ()

type Alt = H.Alt ()
pattern Alt a b c = H.Alt () a b c

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
