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
  File "_build/.dune/default/dune", line 1, characters 0-10:
  1 | (dirs foo)
      ^^^^^^^^^^
  Error: stanza "dirs" may not appear in (include ..) or be generated with
  OCaml syntax dune files
  [1]
