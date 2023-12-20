open Import
module Non_evaluated_rule = Rule
open Memo.O

module Rule = struct
  type t =
    { id : Rule.Id.t
    ; deps : Dep.Set.t
    ; expanded_deps : Path.Set.t
    ; targets : Targets.Validated.t
    ; action : Action.t
    }
end

module Rule_top_closure = Top_closure.Make (Non_evaluated_rule.Id.Set) (Memo)

module rec Expand : sig
  val alias : Alias.t -> Path.Set.t Memo.t
  val deps : Dep.Set.t -> Path.Set.t Memo.t
end = struct
  let alias =
    let memo =
      Memo.create
        "expand-alias"
        ~input:(module Alias)
        (fun alias ->
          let* l =
            Load_rules.get_alias_definition alias
            >>= Memo.parallel_map ~f:(fun (loc, definition) ->
              Memo.push_stack_frame
                (fun () ->
                  Action_builder.evaluate_and_collect_deps
                    (Build_system.dep_on_alias_definition definition)
                  >>| snd)
                ~human_readable_description:(fun () -> Alias.describe alias ~loc))
          in
          let deps = List.fold_left l ~init:Dep.Set.empty ~f:Dep.Set.union in
          Expand.deps deps)
    in
    Memo.exec memo
  ;;

  let deps deps =
    Memo.parallel_map (Dep.Set.to_list deps) ~f:(fun (dep : Dep.t) ->
      match dep with
      | File p -> Memo.return (Path.Set.singleton p)
      | File_selector g ->
        let+ filenames = Build_system.eval_pred g in
        (* Alas, we can't use filename sets here because we end up putting paths coming
           from different directories together. *)
        Path.Set.of_list (Filename_set.to_list filenames)
      | Alias a -> Expand.alias a
      | Env _ | Universe -> Memo.return Path.Set.empty)
    >>| Path.Set.union_all
  ;;
end

let evaluate_rule =
  let memo =
    Memo.create
      "evaluate-rule"
      ~input:(module Non_evaluated_rule)
      (fun rule ->
        let* action, deps = Action_builder.evaluate_and_collect_deps rule.action in
        let* expanded_deps = Expand.deps deps in
        Memo.return
          { Rule.id = rule.id
          ; deps
          ; expanded_deps
          ; targets = rule.targets
          ; action = action.action
          })
  in
  Memo.exec memo
;;

let eval ~recursive ~request =
  let rules_of_deps deps =
    Expand.deps deps
    >>| Path.Set.to_list
    >>= Memo.parallel_map ~f:(fun p ->
      Load_rules.get_rule p
      >>= function
      | None -> Memo.return None
      | Some rule -> evaluate_rule rule >>| Option.some)
    >>| List.filter_opt
  in
  let* (), deps = Action_builder.evaluate_and_collect_deps request in
  let* root_rules = rules_of_deps deps in
  Rule_top_closure.top_closure
    root_rules
    ~key:(fun rule -> rule.Rule.id)
    ~deps:(fun rule -> if recursive then rules_of_deps rule.deps else Memo.return [])
  >>| function
  | Ok l -> l
  | Error cycle ->
    User_error.raise
      [ Pp.text "Dependency cycle detected:"
      ; Pp.chain cycle ~f:(fun rule ->
          Pp.verbatim
            (Path.to_string_maybe_quoted
               (Path.build (Targets.Validated.head rule.targets))))
      ]
;;
