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

The qualified module references are not yet recognized as artifact references:

  $ dune build '%{cmi:Foo.Bar}'
  File "command line", line 1, characters 0-14:
  Error: Module Foo.Bar does not exist.
  [1]
  $ dune build '%{cmi:Foo.Baz}'
  File "command line", line 1, characters 0-14:
  Error: Module Foo.Baz does not exist.
  [1]

Both generated group aliases can coexist:

  $ dune build left.cma right.cma
  $ test -f _build/default/.left.objs/byte/left__Foo.cmi
  $ test -f _build/default/.right.objs/byte/right__Foo.cmi

Nor is there a source module at [Foo]:

  $ dune build '%{cmi:Foo}'
  File "command line", line 1, characters 0-10:
  Error: Module Foo does not exist.
  [1]
