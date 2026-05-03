Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange)
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmt

  $ cat prefix/lib/a/dune-package | grep path
       (source (path A) (impl (path sub/a.ml-gen))))
        (source (path Foo) (impl (path sub/foo.ml))))))

  $ cat >app/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a.sub))
  > EOF

  $ cat > app/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist


  $ ls app/_build/default/dist/node_modules/a.sub
  a.js
  foo.js

The same source paths should drive `node_modules` emission for installed
libraries that use `(include_subdirs qualified)` with renamed directories.

  $ mkdir -p renamed-lib/internal renamed-app renamed-prefix
  $ cat >renamed-lib/dune-project <<EOF
  > (lang dune 3.22)
  > (package (name renamed))
  > (using melange 0.1)
  > EOF
  $ cat >renamed-lib/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (modes melange)
  >  (name renamed)
  >  (public_name renamed))
  > EOF

  $ cat >renamed-lib/internal/leaf.ml <<EOF
  > let value = "from renamed melange dir"
  > EOF

  $ dune build --root renamed-lib @install
  $ dune install --root renamed-lib --prefix $PWD/renamed-prefix --display quiet
  $ cat renamed-prefix/lib/renamed/dune-package | grep "path Public Leaf"
          (source (path Public Leaf) (impl (path internal/leaf.ml))))))))

  $ cat >renamed-app/dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 0.1)
  > EOF
  $ cat >renamed-app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries renamed))
  > EOF

  $ cat >renamed-app/main.ml <<EOF
  > let () = Js.log Renamed.Public.Leaf.value
  > EOF

  $ OCAMLPATH=$PWD/renamed-prefix/lib/:$OCAMLPATH dune build --root renamed-app @dist
  $ find renamed-app/_build/default/dist/node_modules/renamed -type f | sort
  renamed-app/_build/default/dist/node_modules/renamed/internal/leaf.js
  renamed-app/_build/default/dist/node_modules/renamed/renamed.js
  renamed-app/_build/default/dist/node_modules/renamed/renamed__Public.js
  $ node renamed-app/_build/default/dist/main.js
  from renamed melange dir
