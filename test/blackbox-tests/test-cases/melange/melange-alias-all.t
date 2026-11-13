Melange compilation is added to `@all`

  $ make_melange_project 3.21 1.0

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mlib)
  >  (modes melange))
  > EOF
  $ cat > lib/hello.ml <<EOF
  > let x = "hello"
  > EOF

  $ dune build @all
  $ find _build/default | grep '\.cm' | sort
  _build/default/lib/.mlib.objs/melange/mlib.cmi
  _build/default/lib/.mlib.objs/melange/mlib.cmj
  _build/default/lib/.mlib.objs/melange/mlib.cmt
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmi
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmj
  _build/default/lib/.mlib.objs/melange/mlib__Hello.cmt

Empty and interface-only libraries work too. Each nonempty compilation context
contributes its own object directory, even when the libraries share a directory.

  $ mkdir contexts
  $ cat >contexts/dune <<EOF
  > (library
  >  (name empty)
  >  (wrapped false)
  >  (modules)
  >  (modes melange))
  > (library
  >  (name signatures)
  >  (wrapped false)
  >  (modules types)
  >  (modules_without_implementation types)
  >  (modes melange))
  > (library
  >  (name values)
  >  (wrapped false)
  >  (modules value)
  >  (modes melange))
  > EOF
  $ cat >contexts/types.mli <<EOF
  > type t = int
  > EOF
  $ cat >contexts/value.ml <<EOF
  > let value = 42
  > EOF
  $ dune build @contexts/all
  $ test -f _build/default/contexts/.signatures.objs/melange/types.cmi
  $ test ! -e _build/default/contexts/.signatures.objs/melange/types.cmj
  $ test -f _build/default/contexts/.values.objs/melange/value.cmj

The same alias builds qualified modules, generated root and alias modules, and
private modules that are not referenced by the library's public modules.

  $ mkdir -p qualified/sub
  $ cat >qualified/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name qlib)
  >  (libraries mlib)
  >  (private_modules unused)
  >  (root_module root)
  >  (modes melange))
  > EOF
  $ cat >qualified/qlib.ml <<EOF
  > let greeting = Root.Mlib.Hello.x ^ Sub.Leaf.message
  > EOF
  $ cat >qualified/sub/leaf.ml <<EOF
  > let message = " from a qualified module"
  > EOF
  $ cat >qualified/unused.ml <<EOF
  > let value = 42
  > EOF
  $ dune build @qualified/all
  $ test -f _build/default/qualified/.qlib.objs/melange/qlib.cmj
  $ test -f _build/default/qualified/.qlib.objs/melange/qlib__Sub__Leaf.cmj
  $ test -f _build/default/qualified/.qlib.objs/melange/qlib__Unused.cmj
