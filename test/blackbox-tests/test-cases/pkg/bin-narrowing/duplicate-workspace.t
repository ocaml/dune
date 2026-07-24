Two workspace packages installing a binary of the SAME name produce a "more
than one definition" error. But, when only one of them is depended upon the
resolution works correctly.

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

The lookup selects [pkg-a]'s [dup] because it is the only declared dep:

  $ dune build c/dup-out 2>&1

  $ cat _build/default/c/dup-out
  from a
