Demonstrate that it's not possible to generate the (dirs ..) stanza using the
dune syntax.

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ cat >dune <<EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|(dirs foo)|}
  > EOF
  $ dune build
  File "_build/.dune/default/dune", line 1, characters 1-5:
  1 | (dirs foo)
       ^^^^
  Error: Unknown constructor dirs
  [1]
