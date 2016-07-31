# haskell-src-exts-simple

This package provides a shim for haskell-src-exts, exposing the same
AST but without annotations.

This package is useful for synthesizing and manipulating HSE ASTs
that don't carry source location information. It may also aid
in porting packages from haskell-src-exts 1.17 to 1.18, but it
is not a drop-in replacement for the old annotation-free AST.
See [COMPATIBILITY.md](COMPATIBILITY.md) for details.
