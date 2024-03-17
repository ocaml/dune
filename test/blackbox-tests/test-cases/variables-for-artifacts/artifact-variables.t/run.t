Most tests have two versions: one where the variable is used inside a dune file,
and one where the same variables are used in the command line.

  $ export DUNE_SANDBOX=symlink

We begin with a that tries to build several modules defined in the current
directory.

- a belongs to a wrapped library
- b belongs to an executable
- c belongs to an unwrapped library
- d belongs to a wrapped library (transition mode) : in this case, both the
prefixed and unprefixed modules are built.

  $ dune clean
  $ dune build @t1

Command line version.

  $ dune build --verbose %{cmo:a} %{cmo:b} %{cmo:c} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/.a1.objs/byte/a1__A.cmo
  - _build/default/.b.eobjs/byte/dune__exe__B.cmo
  - _build/default/.c1.objs/byte/c.cmo

The next test tries to build a .cmi file (of a module in a wrapped library).

  $ dune clean
  $ dune build @t2

Command line version.

  $ dune build --verbose %{cmi:a} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/.a1.objs/byte/a1__A.cmi

Command line version; note that the error message is slightly different.

  $ dune build %{cmo:xxxx}
  File "command line", line 1, characters 0-11:
  Error: Module Xxxx does not exist.
  [1]

The next test builds a native .cmxa.

  $ dune clean
  $ dune build @t4

Command line version.

  $ dune build --verbose %{cmxa:a1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/a1.cmxa

Command line version.

  $ dune build %{cma:bar_}
  File "command line", line 1, characters 0-11:
  Error: Library bar_ does not exist.
  [1]

This test tries to build a .cma in a subdirectory, where a different project is
defined. The library is public in this case, but we use the local name.

  $ dune clean
  $ dune build @t6

Command line version.

  $ dune build --verbose %{cma:sub2/bar2} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub2/bar2.cma

This test builds a .cmo in a subdirectory (same project).

  $ dune clean
  $ dune build @t7

Command line version.

  $ dune build --verbose %{cmo:sub/x} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub/.bar.objs/byte/bar__X.cmo

This test builds a module in a subdirectory (different project) belonging to a
private library.

  $ dune clean
  $ dune build @t8

COmmand line version.

  $ dune build --verbose %{cmo:sub3/x} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub3/.c1.objs/byte/c1__X.cmo

This test builds a private library in a subdirectory belonging to a different
project.

  $ dune clean
  $ dune build @t9

Command line version.

  $ dune build --verbose %{cma:sub3/c1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub3/c1.cma

This test builds a library in the current directory that has the same name as a
public library defined in a subdirectory.

  $ dune clean
  $ dune build @t10

Command line version.

  $ dune build --verbose %{cma:c1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/c1.cma

This test checks error handling.

  $ dune build %{cma:../x}
  File "command line", line 1, characters 0-11:
  Error: cannot escape the workspace root directory
  [1]
  $ dune build %{cma:../../x}
  Error: path outside the workspace: ../../x from default
  -> required by %{cma:../../x} at command line:1
  [1]

This test checks that everything still works if we invoke dune from a
subdirectory.

  $ (cd sub && dune build %{cmx:x})

The following test checks that the variables can be used in the (action) field
of a (rule).

  $ dune build _build/default/my.cmxs
