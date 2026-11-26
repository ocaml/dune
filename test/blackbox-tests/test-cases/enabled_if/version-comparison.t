Version comparisons use opam ordering, independently of string comparisons.

  $ echo '(lang dune 3.26)' > dune-project
  $ check() {
  >   cat > dune <<EOF
  > (rule
  >  (alias check)
  >  (deps dune)
  >  (enabled_if ($1 $2 $3))
  >  (action (echo enabled)))
  > EOF
  >   dune build @check
  > }

Numeric components are compared numerically.

  $ check 'ver>' 5.10 5.9
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver> 5.10 5.9))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<' 5.9 5.10
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver< 5.9 5.10))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver>' 5.9 5.10
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver> 5.9 5.10))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<' 5.10 5.9
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver< 5.10 5.9))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]

All six operators use version ordering, including equality with leading zeros.

  $ check 'ver=' 5.09 5.9
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver= 5.09 5.9))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<>' 5.09 5.9
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver<> 5.09 5.9))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<>' 5.9 5.10
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver<> 5.9 5.10))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<=' 5.09 5.9
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver<= 5.09 5.9))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver>=' 5.09 5.9
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver>= 5.09 5.9))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver>=' 5.9 5.10
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver>= 5.9 5.10))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver<=' 5.10 5.9
  File "dune", line 4, characters 13-29:
  4 |  (enabled_if (ver<= 5.10 5.9))
                   ^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]

Prereleases sort before releases, while suffixes introduced by '+' sort after.

  $ check 'ver<' 5.9~beta1 5.9
  File "dune", line 4, characters 13-33:
  4 |  (enabled_if (ver< 5.9~beta1 5.9))
                   ^^^^^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ check 'ver>' 5.9+extra 5.9
  File "dune", line 4, characters 13-33:
  4 |  (enabled_if (ver> 5.9+extra 5.9))
                   ^^^^^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]

Variables are expanded before comparing versions.

  $ export DUNE_TEST_VERSION=5.09
  $ check 'ver=' '%{env:DUNE_TEST_VERSION=0}' 5.9
  File "dune", line 4, characters 13-50:
  4 |  (enabled_if (ver= %{env:DUNE_TEST_VERSION=0} 5.9))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]

Ordinary comparisons retain their existing ordering.

  $ check '>' 5.10 5.9
  $ check '<' 5.10 5.9
  enabled
  $ check '=' 5.09 5.9
  $ check '<>' 5.09 5.9
  enabled

Version operators require Dune language 3.26.

  $ echo '(lang dune 3.25)' > dune-project
  $ check 'ver>' 5.10 5.9
  File "dune", line 4, characters 13-28:
  4 |  (enabled_if (ver> 5.10 5.9))
                   ^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]

Package conditions preserve version comparisons in generated opam files.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using unreleased 0.1)
  > (generate_opam_files true)
  > (package
  >  (name example)
  >  (allow_empty)
  >  (enabled_if (ver>= %{os_version} 10.2)))
  > EOF
  $ dune build example.opam
  File "dune-project", line 7, characters 13-39:
  7 |  (enabled_if (ver>= %{os_version} 10.2)))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unexpected list
  [1]
  $ grep '^available:' _build/default/example.opam
  grep: _build/default/example.opam: No such file or directory
  [2]
