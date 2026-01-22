Version generated in opam and META files
----------------------------------------

After calling `dune subst`, dune should embed the version inside the
generated META and opam files.

### With opam files and no package stanzas

  $ cat > dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > EOF

  $ cat > foo.opam <<EOF
  > EOF

  $ cat > dune <<EOF
  > (library (public_name foo))
  > EOF

  $ (git init -q
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build foo.opam META.foo

  $ grep ^version foo.opam
  version: "1.0"

  $ grep ^version _build/default/META.foo
  version = "1.0"

With package stanzas and generating the opam files

  $ cat > dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ (git tag -d 1.0 >/dev/null
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build foo.opam META.foo

  $ grep ^version foo.opam
  version: "1.0"

  $ grep ^version _build/default/META.foo
  version = "1.0"
