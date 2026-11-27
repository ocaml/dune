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
  enabled
  $ check 'ver<' 5.9 5.10
  enabled
  $ check 'ver>' 5.9 5.10
  $ check 'ver<' 5.10 5.9

All six operators use version ordering, including equality with leading zeros.

  $ check 'ver=' 5.09 5.9
  enabled
  $ check 'ver<>' 5.09 5.9
  $ check 'ver<>' 5.9 5.10
  enabled
  $ check 'ver<=' 5.09 5.9
  enabled
  $ check 'ver>=' 5.09 5.9
  enabled
  $ check 'ver>=' 5.9 5.10
  $ check 'ver<=' 5.10 5.9

Prereleases sort before releases, while suffixes introduced by '+' sort after.

  $ check 'ver<' 5.9~beta1 5.9
  enabled
  $ check 'ver>' 5.9+extra 5.9
  enabled

Variables are expanded before comparing versions.

  $ export DUNE_TEST_VERSION=5.09
  $ check 'ver=' '%{env:DUNE_TEST_VERSION=0}' 5.9
  enabled

Both sides must be valid opam versions after expansion.

  $ check 'ver<' '"1.2 beta"' 1.3
  File "dune", line 4, characters 19-29:
  4 |  (enabled_if (ver< "1.2 beta" 1.3))
                         ^^^^^^^^^^
  Error: "1.2 beta" is not a valid version.
  [1]
  $ check 'ver<' 1.0 '""'
  File "dune", line 4, characters 23-25:
  4 |  (enabled_if (ver< 1.0 ""))
                             ^^
  Error: "" is not a valid version.
  [1]
  $ export DUNE_TEST_BAD_VERSION='1.2!'
  $ check 'ver=' '%{env:DUNE_TEST_BAD_VERSION=0}' 1.2
  File "dune", line 4, characters 19-49:
  4 |  (enabled_if (ver= %{env:DUNE_TEST_BAD_VERSION=0} 1.2))
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: "1.2!" is not a valid version.
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
  Error: 'ver>' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
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
  $ grep '^available:' _build/default/example.opam
  available: os-version >= "10.2"
