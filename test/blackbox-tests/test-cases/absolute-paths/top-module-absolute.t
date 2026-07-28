Testing `dune ocaml top-module` with absolute path arguments. Reproduces
#15110 (sub-issue of #12230).

  $ make_dune_project 3.25

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name foolib))
  > EOF
  $ cat > lib/foolib.ml <<EOF
  > let hw () = print_endline "foolib"
  > EOF

A relative path works:

  $ dune ocaml top-module lib/foolib.ml | grep -c "#load"
  1

An absolute path pointing at the same file produces the same output:

  $ dune ocaml top-module $PWD/lib/foolib.ml | grep -c "#load"
  1

A relative path from a subdirectory is misinterpreted: it should resolve
against the invocation cwd, but is treated as workspace-root-relative
(CR-someday Alizter: the next form should print directives for foolib.ml,
identical to running from the workspace root).

  $ (cd lib && dune ocaml top-module foolib.ml --root ..)
  Entering directory '..'
  Error: no module found
  Leaving directory '..'
  [1]

An absolute path that is genuinely outside the workspace produces an
ad-hoc error message rather than the unified "outside the workspace"
wording (CR-someday Alizter: should be "Error: /tmp/does-not-exist.ml is
outside the workspace").

  $ dune ocaml top-module /tmp/does-not-exist.ml
  Error: Module path not a descendent of workspace root.
  [1]
