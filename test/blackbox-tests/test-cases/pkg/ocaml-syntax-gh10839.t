Reproduce #10839.

Dune file in OCaml syntax and a files directory should work

  $ . ./helpers.sh

  $ make_lockdir

  $ make_lockpkg base-bytes <<EOF
  > (version base)
  > 
  > (depends ocamlfind)
  > EOF

  $ make_lockpkg ocamlfind <<EOF
  > (version 1)
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ cat >dune <<EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send ""
  > EOF

  $ dune build

  $ mkdir dune.lock/ocamlfind.files
  $ touch dune.lock/ocamlfind.files/foo.patch

  $ dune build
  Error: Dependency cycle between:
     - evaluating dune file "dune" in OCaml syntax
  -> _build/_private/default/.pkg/ocamlfind/target/cookie
  -> Computing closure for package "base-bytes"
  -> - package base-bytes
  -> lock directory environment for context "default"
  -> base environment for context "default"
  -> loading findlib for context "default"
  -> loading the OCaml compiler for context "default"
  -> - evaluating dune file "dune" in OCaml syntax
  [1]
