When `-x` is passed and a dune file in OCaml syntax is loaded, we should not
crash.
See #9239.

  $ make_dune_project 1.0

  $ cat > dune << EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send ""
  > EOF

  $ dune build -x cross
