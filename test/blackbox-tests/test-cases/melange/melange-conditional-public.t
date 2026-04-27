Show Melange-specific sources are conditionally compiled by Dune in public
libraries.

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package (name pkg))
  > (using melange 0.1)
  > EOF

  $ mkdir a a/b

  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name a)
  >  (public_name pkg)
  >  (modes :standard melange))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "ocaml"
  > EOF
  $ cat > a/b/bar.ml <<EOF
  > let x = "bar"
  > EOF
  $ cat > a/foo.melange.ml <<EOF
  > let x = "melange"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > let x = "melange only file"
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ cat _build/default/a/.melange_src/foo.ml
  # 1 "a/foo.melange.ml"
  let x = "melange"

  $ dune build @install

  $ cat _build/install/default/lib/pkg/foo.ml
  let x = "ocaml"
  $ cat _build/install/default/lib/pkg/melange/foo.ml
  # 1 "a/foo.melange.ml"
  let x = "melange"

  $ find _build/install \( -type f -or -type l \) | sort
  _build/install/default/lib/pkg/META
  _build/install/default/lib/pkg/a.a
  _build/install/default/lib/pkg/a.cma
  _build/install/default/lib/pkg/a.cmi
  _build/install/default/lib/pkg/a.cmt
  _build/install/default/lib/pkg/a.cmx
  _build/install/default/lib/pkg/a.cmxa
  _build/install/default/lib/pkg/a.cmxs
  _build/install/default/lib/pkg/a.ml
  _build/install/default/lib/pkg/a__B.cmi
  _build/install/default/lib/pkg/a__B.cmt
  _build/install/default/lib/pkg/a__B.cmx
  _build/install/default/lib/pkg/a__B__Bar.cmi
  _build/install/default/lib/pkg/a__B__Bar.cmt
  _build/install/default/lib/pkg/a__B__Bar.cmx
  _build/install/default/lib/pkg/a__Foo.cmi
  _build/install/default/lib/pkg/a__Foo.cmt
  _build/install/default/lib/pkg/a__Foo.cmx
  _build/install/default/lib/pkg/b/b.ml
  _build/install/default/lib/pkg/b/bar.ml
  _build/install/default/lib/pkg/dune-package
  _build/install/default/lib/pkg/foo.ml
  _build/install/default/lib/pkg/melange/a.cmi
  _build/install/default/lib/pkg/melange/a.cmj
  _build/install/default/lib/pkg/melange/a.cmt
  _build/install/default/lib/pkg/melange/a.ml
  _build/install/default/lib/pkg/melange/a__B.cmi
  _build/install/default/lib/pkg/melange/a__B.cmj
  _build/install/default/lib/pkg/melange/a__B.cmt
  _build/install/default/lib/pkg/melange/a__B__Bar.cmi
  _build/install/default/lib/pkg/melange/a__B__Bar.cmj
  _build/install/default/lib/pkg/melange/a__B__Bar.cmt
  _build/install/default/lib/pkg/melange/a__Foo.cmi
  _build/install/default/lib/pkg/melange/a__Foo.cmj
  _build/install/default/lib/pkg/melange/a__Foo.cmt
  _build/install/default/lib/pkg/melange/a__Melange_only.cmi
  _build/install/default/lib/pkg/melange/a__Melange_only.cmj
  _build/install/default/lib/pkg/melange/a__Melange_only.cmt
  _build/install/default/lib/pkg/melange/b/b.ml
  _build/install/default/lib/pkg/melange/b/bar.ml
  _build/install/default/lib/pkg/melange/foo.ml
  _build/install/default/lib/pkg/melange/melange_only.ml

