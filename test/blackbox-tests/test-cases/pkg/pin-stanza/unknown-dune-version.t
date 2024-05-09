We are unable to pin projects that the version of dune doesn't understand.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/_b")
  >  (package (name a)))
  > (package
  >  (name main)
  >  (depends a))
  > EOF

  $ mkdir _b
  $ cat >_b/dune-project <<EOF
  > (lang dune 100.1)
  > ;; one day we'll get here
  > EOF

# The location here is messed up b/c we are using a source path (incorrectly)
# to construct the project

  $ dune pkg lock
  File "dune-project", line 1, characters 11-16:
  1 | (lang dune 3.13)
                 ^^^^^
  Error: Version 100.1 of the dune language is not supported.
  Supported versions of this extension in version 100.1 of the dune language:
  - 1.0 to 1.12
  - 2.0 to 2.9
  - 3.0 to 3.16
  [1]
