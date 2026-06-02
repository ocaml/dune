Test `melange.runtime_deps` in a library that has been installed

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir -p lib
  $ echo "Some text" > lib/index.txt
  $ write_melange_dir_target_runtime_deps_lib \
  >   "./some_dir ./index.txt" \
  >   "./some_dir/inside-dir-target.txt"

  $ dune build --root lib

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/index.txt"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/melange/foo.ml" {"melange/foo.ml"}
    "_build/install/default/lib/foo/some_dir/inside-dir-target.txt" {"some_dir/inside-dir-target.txt"}
  ]

  $ cat lib/_build/install/default/lib/foo/dune-package | grep melange_runtime_deps
   (melange_runtime_deps index.txt some_dir))

  $ dune install --root lib --prefix $PWD/prefix

  $ make_melange_app_with_asset_reader app


  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @mel --debug-dependency-path

  $ ls app/_build/default/output/node_modules/foo
  foo.js
  index.txt
  some_dir
