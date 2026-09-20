Menhir inference with ordinary and staged PPX must avoid shadowed aliases (#8989).
The dependency introduced by PPX also shares its name with the root alias Foo.

  $ make_menhir_project 3.25 3.0
  $ mkdir -p ppx lib/group
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name packed_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/packed_ppx.ml <<'EOF'
  > open Ppxlib
  > let packed =
  >   Extension.V3.declare "packed" Extension.Context.expression
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.pexp_ident ~loc
  >         { txt = Longident.Ldot (Longident.Lident "Foo", "packed"); loc })
  > let () =
  >   Driver.register_transformation "packed"
  >     ~rules:[ Context_free.Rule.extension packed ]
  > EOF
  $ cat >lib/group/dune <<'EOF'
  > (menhir
  >  (modules group))
  > EOF
  $ cat >lib/group/foo.ml <<'EOF'
  > module type S = sig end
  > let packed = (module struct end : S)
  > EOF
  $ cat >lib/group/group.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { [%packed] }
  > EOF

  $ build () {
  >   cat >lib/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (preprocess ($1 packed_ppx)))
  > EOF
  >   dune build lib/foo.cma
  > }
  $ build pps
  $ build staged_pps

Both preprocessing modes must also work without generalized opens.

  $ export OCAMLLIB=$(ocamlc -where)
  $ real_ocamlc=$(command -v ocamlc)
  $ mkdir compiler
  $ cat >compiler/ocamlc <<EOF
  > #!/bin/sh
  > case "\$1" in
  >   -config) "$real_ocamlc" "\$@" | sed 's/^version: .*/version: 4.07.1/' ;;
  >   *) exec "$real_ocamlc" "\$@" ;;
  > esac
  > EOF
  $ chmod +x compiler/ocamlc
  $ ln -s "$(command -v ocamldep)" compiler/ocamldep
  $ ln -s "$(command -v ocamlopt)" compiler/ocamlopt
  $ export PATH="$PWD/compiler:$PATH"
  $ build pps
  File "lib/group/group__mock.ml.pp.mock", line 1:
  Error (warning 63 [erroneous-printed-signature]): The printed interface
    differs from the inferred interface. The inferred interface contained items
    which could not be printed properly due to name collisions between
    identifiers. File "_none_", line 1:
    Definition of module Foo__Group__/2
    Beware that this warning is purely informational and will not catch all
    instances of erroneous printed interface.
  [1]
  $ build staged_pps
  File "lib/group/group__mock.ml.mock", line 1:
  Error (warning 63 [erroneous-printed-signature]): The printed interface
    differs from the inferred interface. The inferred interface contained items
    which could not be printed properly due to name collisions between
    identifiers. File "_none_", line 1:
    Definition of module Foo__Group__/2
    Beware that this warning is purely informational and will not catch all
    instances of erroneous printed interface.
  [1]
