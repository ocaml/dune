Pinning dune itself

  $ mkrepo
  $ add_mock_repo_if_needed

# CR-someday rgrinberg: ideally, this source shouldn't be necessary and we
# should disqualify this pin without resolving any sources

  $ mkdir _extra_source
  $ cat >_extra_source/dune-project <<EOF
  > (lang dune 3.12)
  > (package (name dune))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_extra_source")
  >  (package (name dune)))
  > (package
  >   (name main))
  > EOF

For now, pinning dune is not allowed:

  $ dune pkg lock
  File "dune-project", line 4, characters 1-22:
  4 |  (package (name dune)))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: Dune cannot be pinned. The currently running version is the only one
  that may be used
  [1]
