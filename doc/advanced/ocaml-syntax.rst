##############
 OCaml Syntax
##############

..
   TODO(diataxis)
   - reference: files
   - howto: using dynamic features

If a ``dune`` file starts with ``(* -*- tuareg -*- *)``, then it is
interpreted as an OCaml script that generates the ``dune`` file as
described in the rest of this section. The code in the script will have
access to a `Jbuild_plugin
<https://github.com/ocaml/dune/blob/master/plugin/jbuild_plugin.mli>`__
module containing details about the build context it's executed in.

The OCaml syntax gives you an escape hatch for when the S-expression
syntax is not enough. It isn't clear whether the OCaml syntax will be
supported in the long term, as it doesn't work well with incremental
builds. It is possible that it will be replaced by just an ``include``
stanza where one can include a generated file.

Consequently **you must not** build complex systems based on it.
