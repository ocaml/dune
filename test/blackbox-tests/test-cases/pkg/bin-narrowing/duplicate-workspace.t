Two workspace packages installing a binary of the SAME name produce a "more
than one definition" error.

  $ make_lockdir

Two workspace packages, each installing a binary named [dup]:

  $ mkdir -p a b
  $ cat >a/dup.sh <<'EOF'
  > #!/bin/sh
  > echo from a
  > EOF
  $ chmod +x a/dup.sh
  $ cat >a/dune <<'EOF'
  > (install (package pkg-a) (section bin) (files (dup.sh as dup)))
  > EOF
  $ cat >b/dup.sh <<'EOF'
  > #!/bin/sh
  > echo from b
  > EOF
  $ chmod +x b/dup.sh
  $ cat >b/dune <<'EOF'
  > (install (package pkg-b) (section bin) (files (dup.sh as dup)))
  > EOF

A consumer that only depends on [pkg-a]:

  $ mkdir -p c
  $ cat >c/dune <<'EOF'
  > (rule (with-stdout-to dup-out (run %{bin:dup})))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name pkg-a) (allow_empty) (dir a))
  > (package (name pkg-b) (allow_empty) (dir b))
  > (package (name pkg-c) (allow_empty) (dir c) (depends pkg-a))
  > EOF

Today the lookup errors with "more than one definition". Once narrowing lands,
it would instead select [pkg-a]'s [dup], the only declared dep:

  $ dune build c/dup-out 2>&1
  File "b/dune", line 1, characters 47-53:
  1 | (install (package pkg-b) (section bin) (files (dup.sh as dup)))
                                                     ^^^^^^
  Error: binary "dup" is available from more than one definition. It is also
  available in:
  - a/dune:1
  [1]
