The following setup makes the `root_module` stanza export the `Logs`
modules twice, entailing the following error:

```
| File "root.ml-gen", line 15, characters 0-18:
| 15 | module Logs = Logs
|      ^^^^^^^^^^^^^^^^^^
| Error: Multiple definition of the module name Logs.
|        Names must be unique in a given structure or signature.
| [1]
```


Create a dummy library to depend on. The configuration is loosely
based on the logs package, which triggers the issue in real life.

  $ mkdir -p findlib-packages/logs
  $ cat >findlib-packages/logs/META <<EOF
  > description = "Logging infrastructure for OCaml"
  > version = "0.7.0"
  > requires = ""
  > archive(byte) = "logs.cma"
  > archive(native) = "logs.cmxa"
  > plugin(byte) = "logs.cma"
  > plugin(native) = "logs.cmxs"
  > package "lwt" (
  >   description = "Lwt support for Logs"
  >   version = "0.7.0"
  >   requires = "logs"
  >   archive(byte) = "logs_lwt.cma"
  >   archive(native) = "logs_lwt.cmxa"
  >   plugin(byte) = "logs_lwt.cma"
  >   plugin(native) = "logs_lwt.cmxs"
  > )
  > EOF

  $ touch findlib-packages/logs/logs.ml
  $ touch findlib-packages/logs/logs.mli
  $ ocamlc -c findlib-packages/logs/logs.mli -o findlib-packages/logs/logs.cmi
  $ ocamlc -c -I findlib-packages/logs findlib-packages/logs/logs.ml -o findlib-packages/logs/logs.cmo
  $ ocamlc -a findlib-packages/logs/logs.cmo -o findlib-packages/logs/logs.cma

  $ touch findlib-packages/logs/logs_lwt.ml
  $ touch findlib-packages/logs/logs_lwt.mli
  $ ocamlc -c findlib-packages/logs/logs_lwt.mli -o findlib-packages/logs/logs_lwt.cmi
  $ ocamlc -c -I findlib-packages/logs findlib-packages/logs/logs_lwt.ml -o findlib-packages/logs/logs_lwt.cmo
  $ ocamlc -a findlib-packages/logs/logs_lwt.cmo -o findlib-packages/logs/logs_lwt.cma

  $ export OCAMLPATH="./findlib-packages"

Setup the dune project.

  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name root_module)
  >  (root_module root)
  >  (libraries logs.lwt))
  > EOF
  $ cat >root_module.ml <<EOF
  > module Logs = Root.Logs
  > EOF

Trigger the error.

  $ dune build
  File "root.ml-gen", line 3, characters 0-18:
  3 | module Logs = Logs
      ^^^^^^^^^^^^^^^^^^
  Error: Multiple definition of the module name Logs.
         Names must be unique in a given structure or signature.
  [1]
  $ cat _build/default/root.ml-gen
  module Logs = Logs
  module Logs_lwt = Logs_lwt
  module Logs = Logs
  module Logs_lwt = Logs_lwt
