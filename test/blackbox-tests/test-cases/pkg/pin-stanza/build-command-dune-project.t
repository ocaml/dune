Demonstrate the build command we construct for different types of projects:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _template _dune-only _mixed _opam-only

  $ cat >_template/dune-project <<EOF
  > (lang dune 3.13)
  > (generate_opam_files true)
  > (package (name template)  (allow_empty))
  > EOF
  $ cat >_template/mixed.opam.template <<EOF
  > build: [ "echo" "template" ]
  > EOF

  $ cat >_dune-only/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name dune-only)  (allow_empty))
  > EOF

  $ cat >_mixed/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >_mixed/mixed.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "mixed" ]
  > EOF

  $ cat > _opam-only/opam-only.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "opam only" ]
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
  > (pin
  >  (url "$PWD/_opam-only")
  >  (package (name opam-only)))
  > (package
  >  (name main)
  >  (allow_empty)
  >  (depends dune-only mixed template opam-only))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - dune-only.dev
  - mixed.dev
  - opam-only.dev
  - template.dev
  $ build_command() {
  > grep "$1" "${default_lock_dir}/$2.dev.pkg"
  > }
  $ build_command "(dune)" dune-only
   (all_platforms ((dune))))
  $ build_command "(dune)" template
   (all_platforms ((dune))))
  $ build_command "(build" mixed
  (build
  $ build_command "(build" opam-only
  (build

If we build the deps, everything works fine and we see the output of the opam
pins:
  $ dune build @pkg-install
  mixed
  opam only
