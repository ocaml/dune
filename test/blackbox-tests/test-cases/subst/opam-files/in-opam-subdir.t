`dune subst` should work for opam files in opam subdirectory.
See #9862.

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (name pkg)
  > (opam_file_location inside_opam_directory)
  > (package (name pkg))
  > EOF

  $ mkdir opam
  $ cat > opam/pkg.opam << EOF
  > opam-version: "2.0"
  > EOF

Git setup is required for dune subst:

  $ git init -q
  $ git add dune-project opam/pkg.opam
  $ git commit -m message|grep -v root-commit
   2 files changed, 5 insertions(+)
   create mode 100644 dune-project
   create mode 100644 opam/pkg.opam
  $ git tag -a 1.2.3 -m 'tag message'

  $ dune subst

Subst adds a version number based on the git commit:

  $ cat opam/pkg.opam
  version: "1.2.3"
  opam-version: "2.0"
