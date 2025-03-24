open Ppxlib

let rules =
  let extension =
    Extension.declare "c" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
        Ast_builder.Default.eint ~loc [%get_c])
  in
  [ Context_free.Rule.extension extension ]

let () = Ppxlib.Driver.register_transformation "rules" ~rules
