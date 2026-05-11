Building a module artifact target (%{cmi:...}, %{cmo:...}, %{cmx:...},
%{cmt:...}, %{cmti:...}) should work when (include_subdirs qualified) is
used and two qualified subdirectories contain modules with the same leaf
name.

  $ make_dune_project 3.7

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name main) (modes byte native))
  > EOF
  $ cat > main.ml <<EOF
  > let _ = (Sub_a.Group.x, Sub_b.Group.x)
  > EOF

  $ mkdir sub_a sub_b
  $ echo 'let x = "a"' > sub_a/group.ml
  $ echo 'let x = "b"' > sub_b/group.ml

A normal build succeeds:

  $ dune build @check

A module artifact target for the top-level module works:

  $ dune build %{cmi:main}
  $ dune build %{cmo:main}
  $ dune build %{cmx:main}
  $ dune build %{cmt:main}
  $ dune build %{cmti:main}

A module artifact target for a module in a qualified subdirectory works:

  $ dune build %{cmi:sub_a/group}
  $ dune build %{cmi:sub_b/group}
  $ dune build %{cmo:sub_a/group}
  $ dune build %{cmo:sub_b/group}

An unqualified leaf name [group] is not the module path of any module
(the modules are [Sub_a.Group] and [Sub_b.Group]), so it is reported as
missing:

  $ dune build %{cmi:group}
  File "command line", line 1, characters 0-12:
  Error: Module Group does not exist.
  [1]
