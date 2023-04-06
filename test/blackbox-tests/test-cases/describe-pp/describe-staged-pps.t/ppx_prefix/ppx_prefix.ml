open Ppxlib

let prefix_arg = ref "prefixed"

let expand ~ctxt s =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.estring ~loc (!prefix_arg ^ "_" ^ s)

let my_extension =
  Extension.V3.declare "add_prefix" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = 
  Ppxlib.Driver.add_arg "-prefix"
    (Arg.Set_string prefix_arg)
    ~doc:"choose your custom prefix";
  Driver.register_transformation ~rules:[ rule ] "add_prefix"
