We try to use a project that has both opam files and a dune-project file. We
should favor the dune metadata in such a case.

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_source")
  >  (package (name foo))
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends foo bar))
  > EOF

  $ mkdir _source
  $ cat >_source/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > (package (name bar))
  > EOF
  $ cat >_source/bar.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "bar" ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - bar.dev
  - foo.dev

  $  cat dune.lock/bar.pkg | sed "/source/,//d"
  (version dev)
  
  (dune)
  
  
  (dev)
  $  cat dune.lock/foo.pkg | sed "/source/,//d"
  (version dev)
  
  (dune)
  
  
  (dev)
