Demonstrate the build command we construct for different types of projects:

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
  > cat dune.lock/$1.pkg | awk '/build/{ p=1 } /^$/{ if (p) {exit} } { if (p) { print $0 } }'
  > }
  $ build_command "dune-only"
  (build
   (run dune build -p %{pkg-self:name}))
  $ build_command "mixed"
  (build
   (run dune build -p %{pkg-self:name}))
  $ build_command "template"
  (build
   (run dune build -p %{pkg-self:name}))
