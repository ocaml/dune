open Import

let artifacts expander =
  let dir = Expander0.dir expander in
  let open Memo.O in
  let context = Expander0.context expander in
  let* sctx = Super_context.find_exn context in
  Super_context.env_node sctx ~dir >>= Env_node.artifacts
;;

let bin loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      With
        (let open Action_builder.O in
         let prog = Pform.Macro_invocation.Args.whole macro in
         let* artifacts = Action_builder.of_memo (artifacts expander) in
         let* prog =
           Action_builder.of_memo @@ Artifacts.binary ~loc:(Some loc) artifacts prog
         in
         Expander0.Deps.dep (Action.Prog.ok_exn prog)))
;;

let bin_available _loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      Without
        (let open Memo.O in
         let prog = Pform.Macro_invocation.Args.whole macro in
         let* artifacts = artifacts expander in
         let+ available = Artifacts.binary_available artifacts prog in
         [ Value.String (Bool.to_string available) ]))
;;

let vars = [] |> Pform.Var.Map.of_list_exn
let forms = [ Bin, bin; Bin_available, bin_available ] |> Pform.Macro.Map.of_list_exn
let () = Expander0.Source.make vars forms
let linkme = ()
