Test `melange.runtime_deps` for installed libraries where the dune file is
nested more than the dune-project file

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir -p lib/packages/foo/src/runtime
  $ echo "function foo() { return 42; }" > lib/packages/foo/src/runtime/runtime.js
  $ cat > lib/packages/foo/src/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (public_name foo)
  >  (name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./runtime/runtime.js))
  > EOF
  $ cat > lib/packages/foo/src/foo.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/runtime/runtime.js") \`utf8
  > EOF

  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/runtime/runtime.js" {"runtime/runtime.js"}
  ]

  $ grep melange_runtime_deps lib/_build/install/default/lib/foo/dune-package
   (melange_runtime_deps runtime/runtime.js))

  $ cat > lib/packages/foo/src/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (public_name foo)
  >  (name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./runtime/runtime.js))
  > EOF
  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/runtime/runtime.js" {"runtime/runtime.js"}
  ]

  $ grep melange_runtime_deps lib/_build/install/default/lib/foo/dune-package
   (melange_runtime_deps runtime/runtime.js))


