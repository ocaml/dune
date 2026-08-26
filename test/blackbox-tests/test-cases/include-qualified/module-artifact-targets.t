Module artifact targets (%{cmi:...}, %{cmo:...}, %{cmx:...}, %{cmt:...},
%{cmti:...}) use dotted logical module references independently of the Dune
language version. This disambiguates modules with the same leaf name under
(include_subdirs qualified).

  $ make_dune_project 3.24

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name main) (modes byte native))
  > EOF
  $ cat > main.ml <<EOF
  > let _ = (Sub_a.Group.x, Sub_b.Group.x)
  > EOF

  $ mkdir -p foo sub_a/nested sub_b
  $ echo 'let x = "group interface"' > foo/foo.ml
  $ echo 'let x = "a"' > sub_a/group.ml
  $ echo 'let x = "b"' > sub_b/group.ml
  $ echo 'let x = "nested"' > sub_a/nested/group.ml

A normal build succeeds:

  $ dune build @check

A module artifact target for the top-level module works:

  $ dune build %{cmi:main}
  $ dune build %{cmo:main}
  $ dune build %{cmx:main}
  $ dune build %{cmt:main}
  $ dune build %{cmti:main}

A module artifact target for a module in a qualified subdirectory works:

  $ dune build %{cmi:sub_a.group}
  $ dune build %{cmi:Sub_b.Group}
  $ dune build %{cmo:sub_a.group}
  $ dune build %{cmo:Sub_b.Group}

A module group interface is addressed by its logical module reference. An
optional source directory prefix may select the same module tree:

  $ dune build %{cmi:foo}
  $ dune build %{cmi:foo/foo}

An unqualified leaf name [group] is not the module path of any module
(the modules are [Sub_a.Group] and [Sub_b.Group]), so it is reported as
missing:

  $ dune build %{cmi:group}
  File "command line", line 1, characters 0-12:
  Error: Module Group does not exist.
  [1]

A (rule ...) in a subdir dune file uses the full logical module reference:

  $ cat > sub_a/dune <<EOF
  > (rule (with-stdout-to out.txt (echo %{cmi:Sub_a.Nested.Group})))
  > EOF
  $ dune build sub_a/out.txt

The module reference is rooted at the module tree, not relative to the dune
file's directory:

  $ dune build %{cmi:sub_a/Nested.Group}
  File "command line", line 1, characters 0-25:
  Error: Module Nested.Group does not exist.
  [1]

A slash selects a source directory; it does not separate module path
components. Therefore this looks for [Group], not [Sub_a.Group]:

  $ dune build %{cmi:sub_a/group}
  File "command line", line 1, characters 0-18:
  Error: Module reference Group does not match the module at this source path.
  Hint: Sub_a.Group would be a correct module reference
  [1]

A source directory prefix can select the artifacts of a standalone subdirectory:

  $ mkdir -p standalone/sub
  $ cat >standalone/dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ cat >standalone/sub/dune <<'EOF'
  > (library (name bar))
  > EOF
  $ echo 'let x = "standalone"' >standalone/sub/x.ml
  $ dune build --root=standalone %{cmo:sub/x}

Qualified module references require (include_subdirs qualified):

  $ dune build %{cmi:standalone/sub/Foo.Bar}
  File "command line", line 1, characters 0-29:
  Error: Qualified module reference "Foo.Bar" may only be used with
  (include_subdirs qualified).
  [1]
