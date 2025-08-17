Test the tangled source type in project files.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (name foo)
  > (generate_opam_files true)
  > (source (tangled @anil.recoil.org/foo))
  > (package
  >  (allow_empty)
  >  (name foo))
  > EOF

  $ dune build
  $ cat foo.opam | grep -i tangled.sh
  homepage: "https://tangled.sh/@anil.recoil.org/foo"
  bug-reports: "https://tangled.sh/@anil.recoil.org/foo/issues"
  dev-repo: "git+https://tangled.sh/@anil.recoil.org/foo"

The 'tangled' source kind is only supported in Dune lang >=3.21; check that
Dune errors as expected with earlier Dune lang versions.

  $ sed -i -e '1s|.*|(lang dune 3.16)|' dune-project
  $ dune build
  File "dune-project", line 4, characters 8-38:
  4 | (source (tangled @anil.recoil.org/foo))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Tangled is only available since version 3.21 of the dune language.
  Please update your dune-project file to have (lang dune 3.21).
  [1]
