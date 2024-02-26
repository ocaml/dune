Demonstrate the build command we construct for different types of projects:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _template _dune-only _mixed

  $ cat >_template/dune-project <<EOF
  > (lang dune 3.13)
  > (generate_opam_files true)
  > (package (name template))
  > EOF
  $ cat >_template/mixed.opam.template <<EOF
  > build: [ "echo" "template" ]
  > EOF

  $ cat >_dune-only/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name dune-only))
  > EOF

  $ cat >_mixed/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >_mixed/mixed.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "mixed" ]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/_template")
  >  (package (name template)))
  > (pin
  >  (url "$PWD/_dune-only")
  >  (package (name dune-only)))
  > (pin
  >  (url "$PWD/_mixed")
  >  (package (name mixed)))
  > (package
  >  (name main)
  >  (depends dune-only mixed template))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - dune-only.dev
  - mixed.dev
  - template.dev
  $ build_command() {
  > grep "(dune)" dune.lock/$1.pkg
  > }
  $ build_command dune-only
  (dune)
  $ build_command mixed
  (dune)
  $ build_command template
  (dune)
