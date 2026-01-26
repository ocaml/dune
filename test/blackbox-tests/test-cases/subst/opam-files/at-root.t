This test checks that `dune subst` adds a `version:` field to opam files.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (name pkg)
  > EOF

  $ cat > pkg.opam << EOF
  > opam-version: "2.0"
  > EOF

Git setup is required for dune subst:

  $ git init -q
  $ git add dune-project pkg.opam
  $ git commit -m message|grep -v root-commit
   2 files changed, 3 insertions(+)
   create mode 100644 dune-project
   create mode 100644 pkg.opam
  $ git tag -a 1.2.3 -m 'tag message'

  $ dune subst

Subst adds a version number based on the git commit:

  $ cat pkg.opam
  version: "1.2.3"
  opam-version: "2.0"
