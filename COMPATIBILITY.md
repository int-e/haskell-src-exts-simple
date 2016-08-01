# Compatibility notes for haskell-src-exts-simple

## Comparison to haskell-src-exts 1.17 (as of version 1.18):

haskell-src-exts-simple provides a simplified view of the annotated AST in haskell-src-exts, so there are differences to the unannotated AST provided by earlier versions of haskell-src-exts:

* some constructors carried `SrcLoc` information, which is no longer available, notably
  - most constructors of the `Decl`, `ModulePragma`, `ImportDecl`, `IPBind`, `ClassDecl`, `InstDecl`, `QualConDecl`, `GadtDecl` datatypes
  - the `Lambda`, `ExpTypeSig` and `Proc` constructors of the `Exp` type (and a few XML related ones)
  - the `Generator` constructor of `Stmt`
  - the `Alt` constructor of `Alt`
  - the `PatTypeSig` constructor of `Pat`
* the first three fields (not counting the `SrcLoc`) of the `Module` constructor have been combined into a single field with `ModuleHead` type

(this list is probably incomplete)

## ghc versions

* haskell-src-exts-simple requires ghc-7.8 or later
* Note that with ghc-7.8, the constructors of the `Language.Haskell.Exts.Simple.Syntax.Literal` type are only available for pattern matchin, because construction relies on explicitly bidirectional pattern synonyms for literals. For ghc-7.8 compatibility, you should use the `*L` (`intL` etc.) functions for constructing `Literal` values.
