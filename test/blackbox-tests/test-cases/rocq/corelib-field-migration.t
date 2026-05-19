The `(no_corelib)` field makes explicit that this switch controls automatic
Corelib inclusion, not the Rocq Stdlib theory.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using rocq 0.14)
  > EOF
  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (no_corelib))
  > EOF

  $ dune build @all

The new field is presence-only.

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (no_corelib false))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 13-18:
  4 |  (no_corelib false))
                   ^^^^^
  Error: Too many arguments for "no_corelib"
  [1]

The old `(stdlib no)` is rejected and suggests `(no_corelib)`.

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (stdlib no))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 1-12:
  4 |  (stdlib no))
       ^^^^^^^^^^^
  Error: 'stdlib' was deleted in version 0.14 of Rocq Prover build language.
  Use (no_corelib) instead of (stdlib no) or remove the field instead of
  writing (stdlib yes).
  [1]

The old `(stdlib yes)` is rejected and suggests remove the field.

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (stdlib yes))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 1-13:
  4 |  (stdlib yes))
       ^^^^^^^^^^^^
  Error: 'stdlib' was deleted in version 0.14 of Rocq Prover build language.
  Use (no_corelib) instead of (stdlib no) or remove the field instead of
  writing (stdlib yes).
  [1]

When both fields are present, the deleted `(stdlib ...)` field is reported.

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (stdlib no)
  >  (no_corelib))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 1-12:
  4 |  (stdlib no)
       ^^^^^^^^^^^
  Error: 'stdlib' was deleted in version 0.14 of Rocq Prover build language.
  Use (no_corelib) instead of (stdlib no) or remove the field instead of
  writing (stdlib yes).
  [1]

The new field is only available from Rocq language 0.14.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (using rocq 0.13)
  > EOF
  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (modules)
  >  (no_corelib))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 1-13:
  4 |  (no_corelib))
       ^^^^^^^^^^^^
  Error: 'no_corelib' is only available since version 0.14 of Rocq Prover build
  language. Please update your dune-project file to have (using rocq 0.14).
  [1]
