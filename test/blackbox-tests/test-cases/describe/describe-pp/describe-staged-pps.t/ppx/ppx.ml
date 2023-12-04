open Ppxlib

let expand ~ctxt s =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.estring ~loc (s ^ "_suffixed")

let my_extension =
  Extension.V3.declare "add_suffix" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "add_suffix"
