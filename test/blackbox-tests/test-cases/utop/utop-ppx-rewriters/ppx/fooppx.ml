open Ppxlib

let rules =
  let extension =
    Extension.declare "foo" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
      Ast_builder.Default.estring ~loc "PPX extension")
  in
  [ Context_free.Rule.extension extension ]

let () = Ppxlib.Driver.register_transformation "rules" ~rules
