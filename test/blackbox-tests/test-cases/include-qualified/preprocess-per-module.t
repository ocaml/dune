Regression test for GH-15578: qualified modules can be selected in
`(preprocess (per_module ...))`.

  $ make_dune_project 3.25

  $ mkdir foo
  $ touch foo/bar.ml

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) foo.bar))))
  > EOF

  $ dune build

Using a slash instead does not work either:

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) foo/bar))))
  > EOF

  $ dune build
  File "dune", line 7, characters 30-37:
  7 |      (run cat %{input-file})) foo/bar))))
                                    ^^^^^^^
  Error: "foo/bar" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: foobar would be a correct module name
  [1]

Qualified references are only available starting with Dune 3.25:

  $ mkdir version-gate
  $ cd version-gate
  $ make_dune_project 3.24
  $ mkdir foo
  $ touch foo/bar.ml
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) Foo.Bar))))
  > EOF
  $ dune build
  File "dune", line 7, characters 30-37:
  7 |      (run cat %{input-file})) Foo.Bar))))
                                    ^^^^^^^
  Error: Using qualified module references is only available since version 3.25
  of the dune language. Please update your dune-project file to have (lang dune
  3.25).
  [1]
  $ cd ..

They are rejected when the effective `(include_subdirs)` mode is not
`qualified`:

  $ mkdir unqualified
  $ cd unqualified
  $ make_dune_project 3.25
  $ touch bar.ml
  $ cat >dune <<'EOF'
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) Foo.Bar))))
  > EOF
  $ dune build
  File "dune", line 6, characters 30-37:
  6 |      (run cat %{input-file})) Foo.Bar))))
                                    ^^^^^^^
  Error: Qualified module reference "Foo.Bar" may only be used with
  (include_subdirs qualified).
  [1]
  $ cd ..

Unlike legacy `(per_module ...)`, qualified references are also checked against
the modules selected by the stanza:

  $ mkdir missing
  $ cd missing
  $ make_dune_project 3.25
  $ mkdir foo
  $ touch foo/bar.ml
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) Foo.Missing))))
  > EOF
  $ dune build
  File "dune", line 7, characters 30-41:
  7 |      (run cat %{input-file})) Foo.Missing))))
                                    ^^^^^^^^^^^
  Error: Module Foo.Missing doesn't exist.
  [1]
  $ cd ..

The full path distinguishes a root module from a nested module with the same
leaf name. Different preprocessors must be applied to each one:

  $ mkdir exact
  $ cd exact
  $ make_dune_project 3.25
  $ mkdir foo
  $ echo 'This is not OCaml.' >bar.ml
  $ echo 'This is not OCaml either.' >foo/bar.ml
  $ cat >main.ml <<'EOF'
  > let () =
  >   print_endline Refs.Bar.marker;
  >   print_endline Refs.Foo.Bar.marker
  > EOF
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name refs)
  >  (modules Bar Foo.Bar)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"root\"")) Bar)
  >    ((action
  >      (run echo "let marker = \"nested\"")) Foo.Bar))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries refs))
  > EOF
  $ dune exec ./main.exe
  root
  nested
  $ cd ..

A module group interface is named by the group path, without repeating the
last component:

  $ mkdir group-interface
  $ cd group-interface
  $ make_dune_project 3.25
  $ mkdir foo
  $ echo 'This is not OCaml.' >foo/foo.ml
  $ cat >foo/bar.ml <<'EOF'
  > let marker = "nested"
  > EOF
  $ cat >main.ml <<'EOF'
  > let () =
  >   print_endline Group.Foo.marker;
  >   print_endline Group.Foo.Bar.marker
  > EOF
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name group)
  >  (modules Foo Foo.Bar)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"group\"\nmodule Bar = Bar")) Foo))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries group))
  > EOF
  $ dune exec ./main.exe
  group
  nested
  $ cd ..

Modules produced by `(select ...)` participate in qualified reference
resolution:

  $ mkdir selected
  $ cd selected
  $ make_dune_project 3.25
  $ mkdir foo
  $ echo 'This is not OCaml.' >foo/generated.fallback.ml
  $ cat >main.ml <<'EOF'
  > let () = print_endline Selected.Foo.Generated.marker
  > EOF
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name selected)
  >  (modules Foo.Generated)
  >  (libraries
  >   (select foo/generated.ml from
  >    (-> foo/generated.fallback.ml)))
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"selected\"")) Foo.Generated))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries selected))
  > EOF
  $ dune exec ./main.exe
  selected
  $ cd ..

The effective mode of a `(subdir ...)` stanza is used for validation:

  $ mkdir subdir-stanza
  $ cd subdir-stanza
  $ make_dune_project 3.25
  $ mkdir -p nested/foo
  $ echo 'This is not OCaml.' >nested/foo/bar.ml
  $ cat >dune <<'EOF'
  > (subdir nested
  >  (include_subdirs qualified)
  >  (library
  >   (name nested)
  >   (preprocess
  >    (per_module
  >     ((action
  >       (run echo "let marker = \"nested\"")) Foo.Bar)))))
  > EOF
  $ dune build
  $ cd ..

Before Dune 3.25, a single module name keeps its legacy leaf-name semantics:

  $ mkdir legacy
  $ cd legacy
  $ make_dune_project 3.24
  $ mkdir foo
  $ echo 'This is not OCaml.' >foo/bar.ml
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name legacy)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"legacy\"")) Bar))))
  > EOF
  $ dune build
  $ cd ..
