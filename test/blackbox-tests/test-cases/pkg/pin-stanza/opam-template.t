A user may set the build command using the opam template feature. This build
command is currently not respected when the package is pinned.

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ dir=_opam_template

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url $PWD/$dir)
  >  (package (name opam-template)))
  > (package
  >  (name main)
  >  (depends opam-template))
  > EOF

  $ mkdir $dir
  $ cat >$dir/dune-project <<EOF
  > (lang dune 3.13)
  > (generate_opam_files true)
  > (package (allow_empty) (name opam-template))
  > EOF

  $ cat >$dir/opam-template.template <<EOF
  > build: [ "echo" "run" "this" ]
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - opam-template.dev
  $ build_pkg opam-template

  $ cat dune.lock/opam-template.pkg | sed "/source/,//d"
  (version dev)
  
  (dune)
  
