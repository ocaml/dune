Repro for https://github.com/ocaml/dune/issues/10592. Creates some packages
that simulate some of the ocaml compiler packages and test solving a project
that depends on "ocaml".

  $ . ./helpers.sh
  $ mkrepo

  $ CURRENT=5.2.0
  $ NEXT=5.3.0
  $ NEXT_NEXT=5.3.1

The vanilla compiler package, intended to be the default compiler package.
  $ mkpkg ocaml-base-compiler $CURRENT

A configurable compiler. It's marked as avoid-version with the intention that
packages explicitly opt into using it.
  $ mkpkg ocaml-variants $CURRENT+trunk << EOF
  > flags: [avoid-version]
  > EOF

A meta package which depends on a disjunction of different compiler
implementations.
  $ mkpkg ocaml $CURRENT << EOF
  > depends: [
  >   "ocaml-base-compiler" {>= "$CURRENT~" & < "$NEXT~" } |
  >   "ocaml-variants" {>= "$CURRENT~" & < "$NEXT~" }
  > ]
  > EOF

When the latest version of all packages is the same, the ocaml-base-compiler
package is chosen, which is what we want.
  $ solve ocaml
  Solution for dune.lock:
  - ocaml.5.2.0
  - ocaml-base-compiler.5.2.0

Now pretend that there was an alpha release of the current version of the
compiler also in the repo.
  $ mkpkg ocaml-base-compiler $CURRENT+alpha1 << EOF
  > flags: [avoid-version]
  > EOF

The alpha version of the compiler was chosen instead of the stable version.
This would ideally be avoided due to the avoid-version flag, but this flag is
ignored by dune.
  $ solve ocaml
  Solution for dune.lock:
  - ocaml.5.2.0
  - ocaml-base-compiler.5.2.0+alpha1

Now release a new version of ocaml-variants and a new version of ocaml that
uses it. The dependency specification for ocaml is based on how the package is
organized in the wild.
  $ mkpkg ocaml-variants $NEXT+trunk << EOF
  > flags: [avoid-version]
  > EOF
  $ mkpkg ocaml $NEXT << EOF
  > depends: [
  >   "ocaml-base-compiler" {= "$NEXT" } |
  >   "ocaml-variants" {>= "$NEXT~" & < "$NEXT_NEXT~" }
  > ]
  > EOF

Here ocaml-variants is chosen despite its avoid-version flag. This is because
dune currently ignores this flag. This is a problem because the chosen compiler
is not officially released and possibly unstable.
  $ solve ocaml
  Solution for dune.lock:
  - ocaml.5.3.0
  - ocaml-variants.5.3.0+trunk
