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

The dotted module references identify the distinct leaf artifacts:

  $ build_and_show() {
  >   dune build "$@"
  >   dune trace cat | jq 'select(.name == "targets") | .args'
  > }
  $ build_and_show '%{cmi:Foo.Bar}'
  {
    "targets": [
      "_build/default/.left.objs/byte/left__Foo__Bar.cmi"
    ]
  }
  $ build_and_show '%{cmi:Foo.Baz}'
  {
    "targets": [
      "_build/default/.right.objs/byte/right__Foo__Baz.cmi"
    ]
  }

Both generated group aliases can coexist:

  $ dune build left.cma right.cma
  $ test -f _build/default/.left.objs/byte/left__Foo.cmi
  $ test -f _build/default/.right.objs/byte/right__Foo.cmi

The generated group reference itself is ambiguous:

  $ dune build '%{cmi:Foo}'
  File "command line", line 1, characters 0-10:
  Error: Module reference Foo is ambiguous.
  [1]
