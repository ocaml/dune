Qualified module references allow sibling modules in the same directory group
to belong to different stanzas.

  $ make_dune_project 3.25

  $ mkdir foo
  $ cat >foo/bar.ml <<'EOF'
  > let value = "bar"
  > EOF
  $ cat >foo/baz.ml <<'EOF'
  > let value = "baz"
  > EOF

  $ cat >main.ml <<'EOF'
  > let () =
  >   print_endline Bar_lib.Foo.Bar.value;
  >   print_endline Baz_lib.Foo.Baz.value
  > EOF

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name bar_lib)
  >  (modules Foo.Bar))
  > (library
  >  (name baz_lib)
  >  (modules Foo.Baz))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries bar_lib baz_lib))
  > EOF

  $ dune exec ./main.exe
  bar
  baz
