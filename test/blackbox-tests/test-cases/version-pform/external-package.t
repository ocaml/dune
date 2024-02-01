Using a package that is installed needs 2.9 and above

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (universe))
  >  (action (echo foo is '%{version:foo}')))
  > EOF

  $ runtest() {
  > cat >dune-project <<EOF
  > (lang dune $1)
  > EOF
  > dune build @foo
  > }

  $ runtest 2.8
  File "dune", line 4, characters 23-37:
  4 |  (action (echo foo is '%{version:foo}')))
                             ^^^^^^^^^^^^^^
  Error: Package "foo" doesn't exist in the current project.
  Hint: If you want to refer to an installed package, or more generally to a
  package from another project, you need at least (lang dune 2.9).
  [1]
  $ runtest 2.9
  File "dune", line 4, characters 23-37:
  4 |  (action (echo foo is '%{version:foo}')))
                             ^^^^^^^^^^^^^^
  Error: Package "foo" doesn't exist in the current project and isn't installed
  either.
  [1]

Now we install foo without a library
  $ dir=_without_lib
  $ mkdir $dir && cd $dir
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (echo "")))
  > (install (section share) (files foo))
  > EOF
  $ dune build @install
  $ dune install --prefix ./_install
  $ cd ..

  $ OCAMLPATH="$PWD/$dir/_install/lib:$OCAMLPATH" runtest 2.9
  File "dune", line 4, characters 23-37:
  4 |  (action (echo foo is '%{version:foo}')))
                             ^^^^^^^^^^^^^^
  Error: Package "foo" doesn't exist in the current project and isn't installed
  either.
  [1]

Now we install foo with a library
  $ dir=with_lib
  $ mkdir $dir && cd $dir
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat >dune <<EOF
  > (library (public_name foo))
  > EOF
  $ touch foo.ml
  $ dune build @install
  $ dune install --prefix ./_install
  $ cd ..

  $ OCAMLPATH="$PWD/$dir/_install/lib:$OCAMLPATH" runtest 2.9
  foo is ''
