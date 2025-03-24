Package sources can be set to git and be nested:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _repo
  $ cd _repo
  $ git init --quiet
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ mkdir bar
  $ cat >bar/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name bar))
  > EOF
  $ git add -A
  $ git commit -qm "initial commit"
  $ cd ..

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo")
  >  (package (name foo))
  >  (package (name bar)))
  > EOF

  $ dune pkg lock 2>&1 | sed 's#git+file://.*/#$URL#'
  File "dune-project", line 5, characters 1-21:
  5 |  (package (name bar)))
       ^^^^^^^^^^^^^^^^^^^^
  Error: package bar doesn't exist in source
  $URL_repo
