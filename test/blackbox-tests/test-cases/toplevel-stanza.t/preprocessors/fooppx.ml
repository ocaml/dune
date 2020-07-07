open Ppxlib

let rules =
  let extension =
    Extension.declare "test" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
      Ast_builder.Default.eint ~loc 42)
  in
  [ Context_free.Rule.extension extension ]

let () = Ppxlib.Driver.register_transformation "rules" ~rules
