Test errors when public library runtime dependencies escape the dune file dir

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir -p lib/packages/foo/src/
  $ echo "function foo() { return 42; }" > lib/packages/runtime.js
  $ cat > lib/packages/foo/src/dune <<EOF
  > (library
  >  (public_name foo)
  >  (name foo)
  >  (modes melange)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ../../runtime.js))
  > EOF
  $ cat > lib/packages/foo/src/foo.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > EOF

  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

File ends up in the wrong path. Should be `../../runtime.js` from the lib dir

  $ dune install --root lib --prefix "$PWD/_install" foo
  $ ls _install/lib/foo
  META
  dune-package
  foo.ml
  melange
  runtime.js
