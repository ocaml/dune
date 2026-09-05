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

A slash-separated source path is rejected with a hint for the corresponding
logical module reference:

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
  Hint: Foo.Bar would be a correct module reference
  [1]

Qualified references are only available starting with Dune 3.25:

  $ mkdir version-gate
  $ cat >version-gate/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ mkdir version-gate/foo
  $ touch version-gate/foo/bar.ml
  $ cat >version-gate/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) Foo.Bar))))
  > EOF
  $ dune build --root=version-gate
  Entering directory 'version-gate'
  File "dune", line 7, characters 30-37:
  7 |      (run cat %{input-file})) Foo.Bar))))
                                    ^^^^^^^
  Error: Using qualified module references is only available since version 3.25
  of the dune language. Please update your dune-project file to have (lang dune
  3.25).
  Leaving directory 'version-gate'
  [1]

They are rejected when the effective `(include_subdirs)` mode is not
`qualified`:

  $ mkdir unqualified
  $ cat >unqualified/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ touch unqualified/bar.ml
  $ cat >unqualified/dune <<'EOF'
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) Foo.Bar))))
  > EOF
  $ dune build --root=unqualified
  Entering directory 'unqualified'
  File "dune", line 6, characters 30-37:
  6 |      (run cat %{input-file})) Foo.Bar))))
                                    ^^^^^^^
  Error: Qualified module reference "Foo.Bar" may only be used with
  (include_subdirs qualified).
  Leaving directory 'unqualified'
  [1]

The full path distinguishes a root module from a nested module with the same
leaf name. Different preprocessors must be applied to each one:

  $ mkdir exact
  $ cat >exact/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir exact/foo
  $ echo 'This is not OCaml.' >exact/bar.ml
  $ echo 'This is not OCaml either.' >exact/foo/bar.ml
  $ cat >exact/main.ml <<'EOF'
  > let () =
  >   print_endline Refs.Bar.marker;
  >   print_endline Refs.Foo.Bar.marker
  > EOF
  $ cat >exact/dune <<'EOF'
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
  $ dune exec --root=exact ./main.exe
  root
  nested

The full path can also select the sole module of an executable:

  $ mkdir singleton-executable
  $ cat >singleton-executable/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir singleton-executable/foo
  $ echo 'This is not OCaml.' >singleton-executable/foo/main.ml
  $ cat >singleton-executable/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable
  >  (name main)
  >  (modules Foo.Main)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let () = print_endline \"singleton\"")) Foo.Main))))
  > EOF
  $ dune exec --root=singleton-executable ./main.exe
  singleton

A module group interface is named by the group path, without repeating the
last component:

  $ mkdir group-interface
  $ cat >group-interface/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir group-interface/foo
  $ echo 'This is not OCaml.' >group-interface/foo/foo.ml
  $ cat >group-interface/foo/bar.ml <<'EOF'
  > let marker = "nested"
  > EOF
  $ cat >group-interface/main.ml <<'EOF'
  > let () =
  >   print_endline Group.Foo.marker;
  >   print_endline Group.Foo.Bar.marker
  > EOF
  $ cat >group-interface/dune <<'EOF'
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
  $ dune exec --root=group-interface ./main.exe
  group
  nested

Modules produced by `(select ...)` participate in qualified reference
resolution:

  $ mkdir selected
  $ cat >selected/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir selected/foo
  $ echo 'This is not OCaml.' >selected/foo/generated.fallback.ml
  $ cat >selected/main.ml <<'EOF'
  > let () = print_endline Selected.Foo.Generated.marker
  > EOF
  $ cat >selected/dune <<'EOF'
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
  $ dune exec --root=selected ./main.exe
  selected

A selected module can provide a group interface, whose logical path omits the
repeated trie component:

  $ mkdir selected-group
  $ cat >selected-group/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir selected-group/foo
  $ echo 'This is not OCaml.' >selected-group/foo/foo.fallback.ml
  $ cat >selected-group/foo/bar.ml <<'EOF'
  > let marker = "nested"
  > EOF
  $ cat >selected-group/main.ml <<'EOF'
  > let () =
  >   print_endline Selected_group.Foo.marker;
  >   print_endline Selected_group.Foo.Bar.marker
  > EOF
  $ cat >selected-group/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name selected_group)
  >  (modules Foo Foo.Bar)
  >  (libraries
  >   (select foo/foo.ml from
  >    (-> foo/foo.fallback.ml)))
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"group\"\nmodule Bar = Bar")) Foo))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries selected_group))
  > EOF
  $ dune exec --root=selected-group ./main.exe
  group
  nested

The effective mode of a `(subdir ...)` stanza is used for validation:

  $ mkdir subdir-stanza
  $ cat >subdir-stanza/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ mkdir -p subdir-stanza/nested/foo
  $ echo 'This is not OCaml.' >subdir-stanza/nested/foo/bar.ml
  $ cat >subdir-stanza/dune <<'EOF'
  > (subdir nested
  >  (include_subdirs qualified)
  >  (library
  >   (name nested)
  >   (preprocess
  >    (per_module
  >     ((action
  >       (run echo "let marker = \"nested\"")) Foo.Bar)))))
  > EOF
  $ dune build --root=subdir-stanza

Before Dune 3.25, a single module name keeps its legacy leaf-name semantics:

  $ mkdir legacy
  $ cat >legacy/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ mkdir legacy/foo
  $ echo 'This is not OCaml.' >legacy/foo/bar.ml
  $ cat >legacy/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name legacy)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"legacy\"")) Bar))))
  > EOF
  $ dune build --root=legacy
