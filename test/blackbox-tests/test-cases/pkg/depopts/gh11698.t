Reproduce the bug in #11698

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg fmt
  $ mkpkg semver

  $ mkdir _dep
  $ cat >_dep/dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name dep)
  >  (depopts fmt semver))
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.18)
  > (pin
  >  (url "file://$PWD/_dep")
  >  (package
  >   (name dep)
  >   (version 1.0.0)))
  > (package
  >  (name x)
  >  (depends dep))
  > EOF
  Solution for dune.lock:
  - dep.1.0.0
