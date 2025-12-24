Test installed directory targets may be depended on

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.21)
  > (package (name foo))
  > (using directory-targets 0.1)
  > EOF

  $ cat > lib/dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (library (public_name foo) (modes byte))
  > (install (section lib) (dirs some_dir))
  > EOF

  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.cma"
    "_build/install/default/lib/foo/foo.cmi"
    "_build/install/default/lib/foo/foo.cmt"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/some_dir/inside-dir-target.txt" {"some_dir/inside-dir-target.txt"}
  ]

  $ cat lib/_build/install/default/lib/foo/dune-package | grep some_dir
   (lib (META dune-package foo.cma foo.cmi foo.cmt foo.ml (dir some_dir))))

  $ dune install --root lib --prefix $PWD/prefix

  $ cat > app/dune-project <<EOF
  > (lang dune 3.21)
  > (package (name app))
  > EOF
  $ cat > app/dune <<EOF
  > (rule
  >  (target hello.txt)
  >  (deps %{lib:foo:some_dir})
  >  (action (system "cp %{deps}/inside-dir-target.txt %{target}")))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app hello.txt
  Entering directory 'app'
  File "dune", lines 1-4, characters 0-118:
  1 | (rule
  2 |  (target hello.txt)
  3 |  (deps %{lib:foo:some_dir})
  4 |  (action (system "cp %{deps}/inside-dir-target.txt %{target}")))
  Error: File unavailable:
  $TESTCASE_ROOT/prefix/lib/foo/some_dir
  This is not a regular file (S_DIR)
  Leaving directory 'app'
  [1]

  $ cat app/_build/default/hello.txt
  cat: app/_build/default/hello.txt: No such file or directory
  [1]
