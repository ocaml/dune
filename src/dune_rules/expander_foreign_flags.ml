open Import

let make (language : Foreign_language.t) _loc =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      let dir = Expander0.dir expander in
      let result =
        let open Action_builder.O in
        let* context, sctx =
          Action_builder.of_memo
            (let open Memo.O in
             let ctx = Expander0.context expander in
             let* context = Context.DB.get ctx in
             let+ sctx = Super_context.find_exn ctx in
             context, sctx)
        in
        let+ cc =
          let* cc =
            Action_builder.of_memo @@ Super_context.env_node sctx ~dir
            >>| Env_node.foreign_flags
          in
          Foreign_language.Dict.get cc language
        and+ c_compiler =
          let+ ocaml = Action_builder.of_memo @@ Context.ocaml context in
          Ocaml_config.c_compiler ocaml.ocaml_config
        in
        Value.L.strings (c_compiler :: cc)
      in
      With result)
;;

let vars =
  [ Pform.Var.Cc, make Foreign_language.C; Cxx, make Foreign_language.Cxx ]
  |> Pform.Var.Map.of_list_exn
;;

let () = Expander0.Source.make vars Pform.Macro.Map.empty
let linkme = ()
