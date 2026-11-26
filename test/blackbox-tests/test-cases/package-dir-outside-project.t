A package of a nested project cannot claim a directory outside that project.

  $ make_dune_project 3.25
  $ mkdir sub other
  $ cat > sub/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name p) (dir ../other) (allow_empty))
  > EOF
  $ echo '(rule (target x) (action (write-file %{target} x)))' > sub/dune
  $ dune build sub/x @install
  File "sub/dune-project", line 2, characters 23-31:
  2 | (package (name p) (dir ../other) (allow_empty))
                             ^^^^^^^^
  Error: The directory of a package must be inside its project.
  [1]
