The legacy (lang coq) / (using coq ...) Build Language was removed in
Dune 3.24. Projects that still declare it should get a clear error
pointing them at (using rocq ...).

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using coq 0.11)
  > EOF

  $ dune build 2>&1
  File "dune-project", line 2, characters 0-16:
  2 | (using coq 0.11)
      ^^^^^^^^^^^^^^^^
  Error: Extension coq was deleted in the 3.24 version of the dune language
  Hint: The Coq Build Language has been replaced by the Rocq Build Language.
  Use (using rocq <version>) instead.
  [1]
