open Ppxlib

let rules =
  let extension =
    Extension.declare "get_c" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
        Ast_builder.Default.evar ~loc "C.c")
  in
  [ Context_free.Rule.extension extension ]

let () = Ppxlib.Driver.register_transformation "rules" ~rules
