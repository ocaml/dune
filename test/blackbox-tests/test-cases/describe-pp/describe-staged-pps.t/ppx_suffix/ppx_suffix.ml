open Ppxlib

let suffix_arg = ref "suffixed"

let expand ~ctxt s =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.estring ~loc (s ^ "_" ^ !suffix_arg)

let my_extension =
  Extension.V3.declare "add_suffix" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension

let () = 
  Ppxlib.Driver.add_arg "-suffix"
    (Arg.Set_string suffix_arg)
    ~doc:"choose your custom suffix";
  Driver.register_transformation ~rules:[ rule ] "add_suffix"
