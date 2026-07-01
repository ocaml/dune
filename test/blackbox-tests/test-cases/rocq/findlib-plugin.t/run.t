A public Rocq theory in the new findlib layout can depend on a plugin from the
same package. The theory's rocq.d install entries must not be pulled into the
OCAMLPATH layout used for resolving the plugin, otherwise rocqdep sees a cycle
through the theory's own installed .glob file.

  $ make_rocq_project 3.26 0.15
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

The theory package metadata records the plugin dependency.

  $ cat _build/default/META.test
  description = ""
  rocqpath = "test"
  requires = "rocq-core test.plugin"
  package "plugin" (
    directory = "plugin"
    description = ""
    requires = "rocq-runtime.vernac"
    archive(byte) = "plugin.cma"
    archive(native) = "plugin.cmxa"
    plugin(byte) = "plugin.cma"
    plugin(native) = "plugin.cmxs"
  )

The package layout used for Rocq's plugin OCAMLPATH contains plugin files, but
the Rocq rules do not depend on theory install files under rocq.d. Such a
dependency would make rocqdep depend on the theory's own installed .glob file.

  $ dune rules --format=json theories/plugin.vo | jq '
  >   include "dune";
  >   [.[]
  >    | (ruleDepFilePathsOfKind("In_build_dir"),
  >       (ruleDepGlobEntries | .dir))
  >    | select(contains("/rocq.d"))]
  >   | length'
  0
