Generate the source of an executable in a subdir:

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (action (bash "mkdir foo && cat 'print_endline \"42\";;' > foo/bar.ml")))
  > (include_subdirs unqualified)
  > (executable (name bar))
  > EOF

  $ dune exec ./bar.exe
  File "dune", line 5, characters 18-21:
  5 | (executable (name bar))
                        ^^^
  Error: Module "Bar" doesn't exist.
  [1]
