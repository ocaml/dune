Creates some packages that simulate some of the ocaml compiler
packages and test solving a project that depends on "ocaml".

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
  - ocaml-base-compiler.5.2.0
  - ocaml.5.2.0

Now pretend that there was an alpha release of the current version of the
compiler also in the repo.
  $ mkpkg ocaml-base-compiler $CURRENT+alpha1 << EOF
  > flags: [avoid-version]
  > EOF

The alpha version of the compiler is not chosen here because dune's
solver respects the avoid-version flag between multiple versions of
the same package.
  $ solve ocaml
  Solution for dune.lock:
  - ocaml-base-compiler.5.2.0
  - ocaml.5.2.0

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

Note that dune didn't change the solution to include the newest
release of the "ocaml" package, as doing so would cause a dependency
on an unstable version of the compiler. Dune assumes that any version
of compiler packages with a higher version number than the latest
version of ocaml-base-compiler without the avoid-version flag is
unstable.
  $ solve ocaml
  Solution for dune.lock:
  - ocaml-base-compiler.5.2.0
  - ocaml.5.2.0

A package can still force an unstable version of the compiler by leaving no
other choices:

  $ mkpkg edgy 1.0 << EOF
  > depends: [ "ocaml" {> "$CURRENT" } ]
  > EOF
  $ solve edgy
  Solution for dune.lock:
  - edgy.1.0
  - ocaml-variants.5.3.0+trunk (this version should be avoided)
  - ocaml.5.3.0

  $ mkpkg edgy 1.0 << EOF
  > depends: [ "ocaml" "ocaml-base-compiler" {> "$CURRENT" } ]
  > EOF
  $ solve edgy
  Solution for dune.lock:
  - edgy.1.0
  - ocaml-base-compiler.5.2.0+alpha1 (this version should be avoided)
  - ocaml.5.2.0
