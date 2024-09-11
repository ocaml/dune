Test the codeberg source type in project files.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (name foo)
  > (generate_opam_files true)
  > (source (codeberg john/doe))
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

  $ dune build
  $ cat foo.opam | grep -i codeberg.org
  homepage: "https://codeberg.org/john/doe"
  bug-reports: "https://codeberg.org/john/doe/issues"
  dev-repo: "git+https://codeberg.org/john/doe.git"
