The following (failing) test shows that the variables cannot yet be used in the (deps)
field of a (rule).

This test is no longer failing. It should fail because
%{cmo:...} wasn't allowed in the deps field in (lang dune <3.0).

  $ echo "(lang dune 2.1)" > dune-project
  $ cat > dune << EOF
  > (rule
  >  (target t)
  >  (deps %{cmo:x2})
  >  (action (with-stdout-to %{target} (progn))))
  > EOF
  $ dune build t
  File "dune", line 3, characters 7-16:
  3 |  (deps %{cmo:x2})
             ^^^^^^^^^
  Error: Module X2 does not exist.
  [1]

The above restriction also applies to other stanzas. Any stanzas that introduces
new files for Dir_contents, for example copy_files:

  $ cat > dune << EOF
  > (copy_files "%{cmo:x2}")
  > EOF
  $ dune build t
  File "dune", line 1, characters 13-22:
  1 | (copy_files "%{cmo:x2}")
                   ^^^^^^^^^
  Error: %{cmo:..} isn't allowed in this position.
  [1]
