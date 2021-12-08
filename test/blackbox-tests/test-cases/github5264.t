Reproduction case for #5264

Before Dune 3.0, it was possible to abuse the
%{lib:<libname>:<filename>} syntax to refer to the library directory
by writing %{lib:<libname:} or %{lib:<libname>:.}. Several projects
used that in their ctypes rules. Dune 3.0 initially broke this, which
caused many build failures. This test makes sure we still support this
syntax in lang Dune < 3.0.

  $ cat > dune <<'EOF'
  > (library (name ctypes))
  > (rule
  >  (alias a)
  >  (action (run echo %{lib:ctypes:})))
  > (rule
  >  (alias b)
  >  (action (run echo %{lib:ctypes:})))
  > EOF

At the moment, we get a bad error with all language versions:

  $ echo '(lang dune 2.9)' > dune-project
  $ dune build @a
  Error: File unavailable: /home/dim/.opam/4.13.1/lib/ctypes
  This is not a regular file (S_DIR)
  -> required by %{lib:ctypes:} at dune:4
  -> required by alias a in dune:2
  [1]
  $ dune build @b
  Error: File unavailable: /home/dim/.opam/4.13.1/lib/ctypes
  This is not a regular file (S_DIR)
  -> required by %{lib:ctypes:} at dune:7
  -> required by alias b in dune:5
  [1]

  $ echo '(lang dune 3.0)' > dune-project
  $ dune build @a
  Error: File unavailable: /home/dim/.opam/4.13.1/lib/ctypes
  This is not a regular file (S_DIR)
  -> required by %{lib:ctypes:} at dune:4
  -> required by alias a in dune:2
  [1]
  $ dune build @b
  Error: File unavailable: /home/dim/.opam/4.13.1/lib/ctypes
  This is not a regular file (S_DIR)
  -> required by %{lib:ctypes:} at dune:7
  -> required by alias b in dune:5
  [1]
