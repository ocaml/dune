When `-x` is passed and a dune file in OCaml syntax is loaded, we should not
crash.
See #9239.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune << EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send ""
  > EOF

  $ dune build -x cross
