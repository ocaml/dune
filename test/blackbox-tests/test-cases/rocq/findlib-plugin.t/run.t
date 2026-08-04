A public Rocq theory in the new findlib layout can depend on a plugin from the
same package. The theory's rocq.d install entries must not be pulled into the
OCAMLPATH layout used for resolving the plugin, otherwise rocqdep sees a cycle
through the theory's own installed .glob file.

  $ make_rocq_project 3.25 0.15
  $ cat >> dune-project << EOF
  > (name test)
  > (generate_opam_files true)
  > (package (name test))
  > EOF

  $ mkdir plugin theories
  $ cat > plugin/dune << EOF
  > (library
  >  (name plugin)
  >  (public_name test.plugin)
  >  (libraries rocq-runtime.vernac))
  > 
  > (rocq.pp (modules g_test))
  > EOF
  $ touch plugin/plugin.mlpack
  $ cat > plugin/g_test.mlg << EOF
  > DECLARE PLUGIN "rocq-term-deps.plugin"
  > 
  > {
  > 
  > [@@@ocaml.warning "-27"]
  > 
  > open Stdarg
  > 
  > }
  > 
  > VERNAC COMMAND EXTEND TermDeps CLASSIFIED AS QUERY
  > | [ "Hello" ] -> { () }
  > END
  > EOF

  $ cat > theories/dune << EOF
  > (rocq.theory
  >  (public_name test)
  >  (name test)
  >  (plugins test.plugin))
  > EOF
  $ cat > theories/plugin.v << EOF
  > Declare ML Module "test.plugin".
  > EOF

  $ dune build theories/plugin.vo

The package layout used for Rocq's plugin OCAMLPATH contains plugin files, but
not Rocq theory install files. Exposing the theory's rocq.d symlinks here would
let rocqdep depend on the theory's own installed .glob file.

  $ find _build/install/default/.packages-rocq -path '*rocq.d*' -print
