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

Asking for any module artifact of a module in the stanza crashes with the
same internal error:

  $ dune build %{cmi:main} 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "Group" })
  [1]

  $ dune build %{cmo:main} 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "Group" })
  [1]

  $ dune build %{cmx:main} 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "Group" })
  [1]

  $ dune build %{cmt:main} 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "Group" })
  [1]

  $ dune build %{cmti:main} 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "Group" })
  [1]
