Reproduction case for #5264

Before Dune 3.0, it was possible to abuse the
%{lib:<libname>:<filename>} syntax to refer to the library directory
by writing %{lib:<libname:} or %{lib:<libname>:.}. Several projects
used that in their ctypes rules. Dune 3.0 initially broke this, which
caused many build failures. This test makes sure we still support this
syntax in lang Dune < 3.0.

  $ cat > dune <<'EOF'
  > (library (name ctypes))
  > (library (name other))
  > (rule
  >  (alias a)
  >  (action (run echo "ctypes:" %{lib-private:ctypes:})))
  > (rule
  >  (alias b)
  >  (action (run echo "other:" %{lib-private:other:.})))
  > EOF

We still support it with older version of the language, for backward
compatibility purposes:

  $ echo '(lang dune 2.9)' > dune-project
  $ dune build @a
  ctypes: .
  $ dune build @b
  other: .

But we are more strict since 3.0:

  $ echo '(lang dune 3.0)' > dune-project
  $ dune build @a
  File "dune", line 5, characters 29-51:
  5 |  (action (run echo "ctypes:" %{lib-private:ctypes:})))
                                   ^^^^^^^^^^^^^^^^^^^^^^
  Error: The form %{lib-private:<libname>:} is no longer supported since
  version 3.0 of the Dune language.
  Hint: Did you know that Dune 3.0 supports ctypes natively? See the manual for
  more details.
  [1]
  $ dune build @b
  File "dune", line 8, characters 28-50:
  8 |  (action (run echo "other:" %{lib-private:other:.})))
                                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: The form %{lib-private:<libname>:.} is no longer supported since
  version 3.0 of the Dune language.
  Hint: If you are trying to use this form to include a directory, you should
  instead use (foreign_stubs (include_dirs (lib other))). See the manual for
  more details.
  [1]
