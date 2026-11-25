@doc-private skips a private library with no compilation modes and still builds
HTML for the other private library.

  $ make_dune_project 3.13

  $ cat > dune <<EOF
  > (library
  >  (name normal)
  >  (modules normal))
  > (library
  >  (name skipped)
  >  (modules skipped)
  >  (modes))
  > EOF
  $ echo 'let x = 1' > normal.ml
  $ cp normal.ml skipped.ml

  $ dune build @doc-private
  $ find _build/default/_doc/_html -name index.html | LC_ALL=C sort \
  >   | dune_cmd subst '@[0-9a-f]+/' '@<scope>/'
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/normal@<scope>/Normal/index.html

@doc-private also documents a package-less implementation of a virtual
library. Check for HTML for a module defined by the implementation itself.

  $ touch example.opam
  $ mkdir vlib impl
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (public_name example)
  >  (wrapped false)
  >  (virtual_modules v))
  > EOF
  $ echo 'val x : int' > vlib/v.mli
  $ echo '(library (name impl) (implements vlib))' > impl/dune
  $ echo 'let x = 1' > impl/v.ml
  $ echo 'let y = 2' > impl/extra.ml

Ignore odoc's warning about using V's .cmt instead of the .cmti, which
belongs to the virtual library.

  $ dune build @impl/doc-private 2>&1 \
  >   | awk '!/^WARNING: not processing the "interface" file\./'
  $ test -f _build/default/_doc/_html/impl@*/Extra/index.html
