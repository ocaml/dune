open Import
open Memo.O

let expand loc macro artifacts_host =
  let s = Pform.Macro_invocation.Args.whole macro in
  let* coqc = Artifacts.binary artifacts_host ~loc:(Some loc) "coqc" in
  let+ t = Coq_config.make ~coqc in
  match t with
  | Error msg ->
    User_error.raise
      ~loc
      [ Pp.textf "Could not expand %%{coq:%s} as running coqc --config failed." s; msg ]
      ~hints:
        [ Pp.textf
            "coqc --config requires the coq-stdlib package in order to function properly."
        ]
  | Ok t ->
    (match Coq_config.by_name t s with
     | None ->
       User_error.raise ~loc [ Pp.textf "Unknown Coq configuration variable %S" s ]
     | Some v ->
       (match v with
        | Int x -> [ Dune_lang.Value.String (string_of_int x) ]
        | String x -> [ String x ]
        | Path x -> Dune_lang.Value.L.paths [ x ]))
;;

let () =
  let expand loc (macro : Pform.Macro_invocation.t) =
    Expander0.Expansion_result.Need_full_expander
      (fun t ->
        Expander0.Deps.Without
          (let context = Expander0.context t in
           let dir = Expander0.dir t in
           let* sctx = Super_context.find_exn context in
           let* artifacts_host = Super_context.artifacts_host sctx ~dir in
           expand loc macro artifacts_host))
  in
  Expander0.Source.make
    Pform.Var.Map.empty
    (Pform.Macro.Map.of_list_exn [ Pform.Macro.Coq_config, expand ])
;;

let linkme = ()
