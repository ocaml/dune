With (odoc auto), the generated opam file depends on odoc and builds the @doc
alias only when the package has something to document: a library or a
documentation stanza. This is the default from lang 3.26 with the unreleased
extension, which gen enables.

  $ gen() {
  > rm -f foo.opam
  > cat > dune-project << EOF
  > (lang dune $1)
  > (using unreleased 0.1)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty) $2)
  > EOF
  > dune build foo.opam &&
  > { grep -e odoc -e @doc _build/default/foo.opam || echo no odoc; }
  > }

Before lang 3.26, the default is still (odoc true):

  $ gen 3.22 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

(odoc auto) can be set explicitly. An empty package has no documentation:

  $ gen 3.22 '(odoc auto)'
  File "dune-project", line 4, characters 40-44:
  4 | (package (name foo) (allow_empty) (odoc auto))
                                              ^^^^
  Error: Unknown value auto
  [1]

From lang 3.26, with the unreleased extension, (odoc auto) is the default:

  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

(odoc true) still forces the documentation:

  $ gen 3.26 '(odoc true)'
    "odoc" {with-doc}
      "@doc" {with-doc}

An executable is not documented:

  $ cat > dune << EOF
  > (executable (public_name foo))
  > EOF
  $ touch foo.ml
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

A public library is documented:

  $ cat > dune << EOF
  > (library (public_name foo))
  > EOF
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

A private library without a package field is not documented, as only
@doc-private documents it:

  $ cat > dune << EOF
  > (library (name foo))
  > EOF
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

A private library attached to the package is documented:

  $ cat > dune << EOF
  > (library (name foo) (package foo))
  > EOF
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

And a documentation stanza:

  $ rm foo.ml
  $ cat > dune << EOF
  > (documentation (package foo))
  > EOF
  $ cat > foo.mld << EOF
  > {0 Foo}
  > EOF
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}

(odoc false) still disables the documentation:

  $ gen 3.26 '(odoc false)'
  no odoc

The detection does not depend on the package's enabled_if, as the opam file
must be the same on every system:

  $ rm foo.mld
  $ cat > dune << EOF
  > (library (public_name foo))
  > EOF
  $ touch foo.ml
  $ gen 3.26 '(enabled_if (= %{architecture} nonexistent))'
    "odoc" {with-doc}
      "@doc" {with-doc}

Without the unreleased extension, the default remains (odoc true), even from
lang 3.26:

  $ rm dune foo.ml
  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty))
  > EOF
  $ dune build foo.opam && grep -e odoc -e @doc _build/default/foo.opam
    "odoc" {with-doc}
      "@doc" {with-doc}

dune describe opam-files does not need a context named default:

  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty))
  > EOF
  $ cat > dune-workspace << EOF
  > (lang dune 3.26)
  > (context (default (name alt)))
  > EOF
  $ dune describe opam-files > out 2>&1
  $ grep -o -e 'Internal error' -e opam-version -e '{with-doc}' out
  opam-version
  {with-doc}
  {with-doc}
  $ rm dune-workspace

The opam file written by dune init matches the one generated afterwards:

  $ dune init proj --kind=executable myexe > /dev/null
  Entering directory 'myexe'
  Success: initialized project component named myexe
  Leaving directory 'myexe'
  $ (cd myexe && dune build @install)

Libraries brought in by include or defined in a subdir stanza are detected:

  $ mkdir include && cd include
  $ cat > dune << EOF
  > (include dune.inc)
  > EOF
  $ cat > dune.inc << EOF
  > (library (public_name foo))
  > EOF
  $ touch foo.ml
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}
  $ cd ..

  $ mkdir subdir && cd subdir
  $ cat > dune << EOF
  > (subdir sub (library (public_name foo)))
  > EOF
  $ mkdir sub && touch sub/foo.ml
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}
  $ cd ..

The stanzas of a dune file in OCaml syntax depend on the build context, so all
the packages of its project are considered documented:

  $ mkdir ocaml-syntax && cd ocaml-syntax
  $ cat > dune << EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send "(library (public_name foo))"
  > EOF
  $ touch foo.ml
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}
  $ cd ..

So are those of a dynamic include:

  $ mkdir dynamic-include && cd dynamic-include
  $ mkdir gen doc
  $ cat > gen/dune << EOF
  > (rule (with-stdout-to dune.inc (echo "(documentation (package foo))")))
  > EOF
  $ cat > doc/dune << EOF
  > (dynamic_include ../gen/dune.inc)
  > EOF
  $ cat > doc/index.mld << EOF
  > {0 Foo}
  > EOF
  $ gen 3.26 ''
    "odoc" {with-doc}
      "@doc" {with-doc}
  $ cd ..

--only-packages does not change the result:

  $ mkdir only-packages && cd only-packages
  $ cat > dune-project << EOF
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty))
  > (package (name bar) (allow_empty))
  > EOF
  $ cat > dune << EOF
  > (library (public_name foo))
  > (executable (public_name bar) (package bar))
  > EOF
  $ touch foo.ml bar.ml
  $ dune build --only-packages foo foo.opam && grep -c with-doc _build/default/foo.opam
  2
  $ dune build --only-packages bar bar.opam && grep -c with-doc _build/default/bar.opam
  2
  $ cd ..
