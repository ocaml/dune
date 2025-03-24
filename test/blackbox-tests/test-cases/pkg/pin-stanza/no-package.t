Here we try to pin a package to a source that doesn't define said package:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _bar
  $ cat >_bar/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name no-bar))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/_bar")
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends bar))
  > EOF

  $ dune pkg lock 2>&1 | sed 's#file://.*#$URL#g'
  File "dune-project", line 4, characters 1-21:
  4 |  (package (name bar)))
       ^^^^^^^^^^^^^^^^^^^^
  Error: package bar doesn't exist in source
  $URL
