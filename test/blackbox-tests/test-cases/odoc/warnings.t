This project makes odoc warnings fatal at the root in the dev profile. A
nested env makes compile and link warnings nonfatal. Compile warnings also
remain nonfatal in the release profile.

  $ make_dune_project 2.4
  $ touch foo_doc.opam foo_lib.opam bar_doc.opam bar_lib.opam baz_lib.opam
  $ mkdir -p foo_doc foo_lib sub_env/bar_doc sub_env/bar_lib sub_env/baz_lib

  $ cat > dune <<EOF
  > (env
  >  (dev
  >   (odoc
  >    (warnings fatal))))
  > EOF
  $ cat > sub_env/dune <<EOF
  > (env
  >  (dev
  >   (odoc
  >    (warnings nonfatal))))
  > EOF

  $ echo '(documentation (package foo_doc))' > foo_doc/dune
  $ printf '{0 Foo}\n\n[doc\n' > foo_doc/foo.mld
  $ echo '(documentation (package bar_doc))' > sub_env/bar_doc/dune
  $ printf '{0 Bar}\n\n[doc\n' > sub_env/bar_doc/bar.mld

  $ cat > foo_lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes byte)
  >  (public_name foo_lib)
  >  (modules_without_implementation foo))
  > EOF
  $ cat > foo_lib/foo.mli <<EOF
  > (** [t *)
  > type t = int
  > EOF

  $ cat > sub_env/bar_lib/dune <<EOF
  > (library
  >  (name bar)
  >  (modes byte)
  >  (public_name bar_lib)
  >  (modules_without_implementation bar))
  > EOF
  $ cp foo_lib/foo.mli sub_env/bar_lib/bar.mli

  $ cat > sub_env/baz_lib/dune <<EOF
  > (library
  >  (name baz)
  >  (modes byte)
  >  (public_name baz_lib)
  >  (modules_without_implementation baz))
  > EOF
  $ cat > sub_env/baz_lib/baz.mli <<EOF
  > (** See {!Baz.missing}. *)
  > type t = int
  > EOF

  $ export BUILD_PATH_PREFIX_MAP=odoc=`command -v odoc`

A syntax warning in a package page fails the build:

  $ dune build --only-packages=foo_doc @doc
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

The same policy applies to doc comments in .mli files:

  $ dune build --only-packages=foo_lib @doc
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

The nested env reports compile warnings without failing the build:

  $ dune build --only-packages=bar_doc,bar_lib @doc
  File "../../../../sub_env/bar_doc/bar.mld", line 4, characters 0-0:
  Warning: End of text is not allowed in '[...]' (code).
  File "sub_env/bar_lib/bar.mli", line 1, characters 7-7:
  Warning: End of text is not allowed in '[...]' (code).

An unresolved reference produces a nonfatal link warning in the nested env:

  $ dune build --only-packages=baz_lib @doc 2>&1 \
  >   | dune_cmd subst 'unresolvedroot\(Baz\)' Baz
  File "sub_env/baz_lib/baz.mli", line 1, characters 8-22:
  Warning: Failed to resolve reference Baz.missing Couldn't find "missing"

In the release profile, warnings at the root are nonfatal:

  $ dune build -p foo_doc,foo_lib @doc 2>&1 | censor
  (cd _build/.sandbox/$DIGEST1/default/_doc/_odoc/pkg/foo_doc && odoc compile --pkg foo_doc -o page-foo.odoc ../../../../foo_doc/foo.mld)
  File "../../../../foo_doc/foo.mld", line 4, characters 0-0:
  Warning: End of text is not allowed in '[...]' (code).
  (cd _build/.sandbox/$DIGEST2/default/foo_lib/.foo.objs/byte && odoc compile -I . -I ../../../_doc/_odoc/pkg/foo_lib --pkg foo_lib -o foo.odoc foo.cmti)
  File "foo_lib/foo.mli", line 1, characters 7-7:
  Warning: End of text is not allowed in '[...]' (code).
