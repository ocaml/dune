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

  $ mkdir ${default_lock_dir}/ocamlfind.files
  $ touch ${default_lock_dir}/ocamlfind.files/foo.patch

  $ dune build
