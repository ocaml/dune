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

The 'codeberg' source kind is only supported in Dune lang >=3.17; check that
Dune errors as expected with earlier Dune lang versions.

  $ sed -i -e '1s|.*|(lang dune 3.16)|' dune-project
  $ dune build
  File "dune-project", line 4, characters 8-27:
  4 | (source (codeberg john/doe))
              ^^^^^^^^^^^^^^^^^^^
  Error: Codeberg is only available since version 3.17 of the dune language.
  Please update your dune-project file to have (lang dune 3.17).
  [1]
