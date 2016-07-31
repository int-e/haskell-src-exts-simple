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

* haskell-src-exts-simple requires ghc-7.10 or later, because it relies on bidirectional pattern synonyms
