Two libraries may own different modules in the same qualified group. Although
they each generate a [Foo] alias, their leaf module artifacts are distinct.

  $ make_dune_project 3.25

  $ mkdir foo
  $ cat >foo/bar.unix.ml <<'EOF'
  > let value = "bar"
  > EOF
  $ cat >foo/baz.unix.ml <<'EOF'
  > let value = "baz"
  > EOF

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name left)
  >  (libraries
  >   (select foo/bar.ml from
  >    (unix -> foo/bar.unix.ml))))
  > (library
  >  (name right)
  >  (libraries
  >   (select foo/baz.ml from
  >    (unix -> foo/baz.unix.ml))))
  > EOF

The source-path artifact references remain unambiguous:

  $ build_and_show() {
  >   dune build "$@"
  >   dune trace cat | jq 'select(.name == "targets") | .args'
  > }
  $ build_and_show '%{cmi:foo/bar}'
  {
    "targets": [
      "_build/default/.left.objs/byte/left__Foo__Bar.cmi"
    ]
  }
  $ build_and_show '%{cmi:foo/baz}'
  {
    "targets": [
      "_build/default/.right.objs/byte/right__Foo__Baz.cmi"
    ]
  }

Both generated group aliases can coexist:

  $ dune build left.cma right.cma
  $ test -f _build/default/.left.objs/byte/left__Foo.cmi
  $ test -f _build/default/.right.objs/byte/right__Foo.cmi

There is no unique source-path artifact reference for the generated group:

  $ dune build '%{cmi:foo}'
  File "command line", line 1, characters 0-10:
  Error: Module Foo does not exist.
  [1]
