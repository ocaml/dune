open Import
module Non_evaluated_rule = Rule
module Non_evaluated_anon = Rule.Anonymous_action
open Memo.O

module Rule = struct
  type t =
    { id : Digest.t
    ; deps : Dep.Set.t
    ; expanded_deps : Path.Set.t
    ; targets : Targets.Validated.t option
    ; action : Action.t
    ; aliases : Alias_name.Set.t option
    ; loc : Loc.t
    }

  let alias_list { aliases; _ } =
    Option.map aliases ~f:(Alias_name.Set.to_list_map ~f:Alias_name.to_string)
  ;;

  let pp t =
    let { targets; loc; _ } = t in
    let name =
      match targets with
      | Some targets ->
        Targets.Validated.head targets |> Path.build |> Path.to_string_maybe_quoted
      | None ->
        let alias = alias_list t in
        (match alias with
         | Some (alias :: _) -> "alias " ^ alias
         | Some [] | None -> "anonymous action")
    in
    let loc =
      if Loc.is_none loc
      then ""
      else (
        let start = Loc.start loc in
        sprintf " in %s:%d" start.pos_fname start.pos_lnum)
    in
    Pp.verbatim (name ^ loc)
  ;;
end

(* A rule is identified by its first target and an anonymous action by the stamp
   file the build system executes it through. The latter is named after a digest
   of the evaluated action, so an action attached to several aliases has a single
   identity here, just as it has a single execution there. *)
let digest_of_identity path = Digest.string (Path.Build.to_string path)

module Anon_rules = struct
  type t = Rule.t Digest.Map.t

  let empty = Digest.Map.empty

  let combine =
    Digest.Map.union ~f:(fun _ (a : Rule.t) (b : Rule.t) ->
      let aliases = Option.merge a.aliases b.aliases ~f:Alias_name.Set.union in
      Some { a with Rule.aliases })
  ;;

  module Aliases = struct
    type t = Alias_name.Set.t Digest.Map.t ref

    let create () = ref Digest.Map.empty

    let update ~anon_rules (aliases : t) =
      aliases
      := Digest.Map.foldi anon_rules ~init:!aliases ~f:(fun id (rule : Rule.t) acc ->
           Digest.Map.update acc id ~f:(fun existing ->
             Option.merge existing rule.aliases ~f:Alias_name.Set.union))
    ;;

    let apply ~rules (aliases : t) =
      List.map rules ~f:(fun (rule : Rule.t) ->
        match Digest.Map.find !aliases rule.id with
        | None -> rule
        | Some aliases -> { rule with Rule.aliases = Some aliases })
    ;;
  end
end

module Rule_top_closure = Top_closure.Make (Digest.Set) (Memo)

module rec Expand : sig
  val alias : Alias.t -> (Path.Set.t * Anon_rules.t) Memo.t
  val deps : Dep.Set.t -> (Path.Set.t * Anon_rules.t) Memo.t
end = struct
  let empty = Path.Set.empty, Anon_rules.empty

  let combine (paths, anon_rules) (paths', anon_rules') =
    Path.Set.union paths paths', Anon_rules.combine anon_rules anon_rules'
  ;;

  let anonymous_action alias (anon : Non_evaluated_anon.t) =
    let* full_action, deps = Action_builder.evaluate_and_collect_deps anon.action in
    let stamp_file = Build_system.anonymous_action_stamp_file anon ~full_action ~deps in
    let+ expanded_deps, _anon_rules = Expand.deps deps in
    let id = digest_of_identity stamp_file in
    ( Path.Set.empty
    , Digest.Map.singleton
        id
        { Rule.id
        ; deps
        ; expanded_deps
        ; targets = None
        ; action = full_action.action
        ; aliases = Some (Alias_name.Set.singleton alias)
        ; loc = anon.loc
        } )
  ;;

  let alias =
    let memo =
      Memo.create
        "expand-alias"
        ~input:(module Alias)
        (fun alias ->
           Load_rules.get_alias_definition alias
           >>= Memo.map_reduce ~empty ~combine ~f:(fun (loc, definition) ->
             Memo.push_stack_frame
               (fun () ->
                  match (definition : Rules.Dir_rules.Alias_spec.item) with
                  | Deps action_builder ->
                    let* (), deps =
                      Action_builder.evaluate_and_collect_deps action_builder
                    in
                    Expand.deps deps
                  | Action anon -> anonymous_action (Alias.name alias) anon)
               ~human_readable_description:(fun () -> Alias.describe alias ~loc)))
    in
    Memo.exec memo
  ;;

  let deps deps =
    Memo.map_reduce (Dep.Set.to_list deps) ~empty ~combine ~f:(fun (dep : Dep.t) ->
      match dep with
      | File p -> Memo.return (Path.Set.singleton p, Anon_rules.empty)
      | File_selector g ->
        let+ filenames = Build_system.eval_pred g in
        (* Alas, we can't use filename sets here because we end up putting paths coming
           from different directories together. *)
        Path.Set.of_list (Filename_set.to_list filenames), Anon_rules.empty
      | Alias a -> Expand.alias a
      | Env _ | Universe -> Memo.return empty)
  ;;
end

let evaluate_rule =
  let memo =
    Memo.create
      "evaluate-rule"
      ~input:(module Non_evaluated_rule)
      ~initial_store_size:4096
      (fun rule ->
         let* action, deps = Action_builder.evaluate_and_collect_deps rule.action in
         let* expanded_deps, _anon_rules = Expand.deps deps in
         Memo.return
           { Rule.id = digest_of_identity (Targets.Validated.head rule.targets)
           ; deps
           ; expanded_deps
           ; targets = Some rule.targets
           ; action = action.action
           ; aliases = None
           ; loc = Non_evaluated_rule.loc rule
           })
  in
  Memo.exec memo
;;

let eval ~recursive ~request =
  (* The same anonymous action can be attached to several aliases, in which case
     the closure below visits it once and forgets all but the first of its
     aliases. We accumulate them here and put them back once it is done. *)
  let aliases = Anon_rules.Aliases.create () in
  let rules_of_deps deps =
    let* paths, anon_rules = Expand.deps deps in
    Anon_rules.Aliases.update ~anon_rules aliases;
    let+ rules =
      Path.Set.to_list paths
      |> Memo.parallel_map ~f:(fun p ->
        Load_rules.get_rule p
        >>= function
        | None -> Memo.return None
        | Some rule -> evaluate_rule rule >>| Option.some)
      >>| List.filter_opt
    in
    rules @ Digest.Map.values anon_rules
  in
  let* (), deps = Action_builder.evaluate_and_collect_deps request in
  let* root_rules = rules_of_deps deps in
  Rule_top_closure.top_closure
    root_rules
    ~key:(fun (rule : Rule.t) -> rule.id)
    ~deps:(fun rule -> if recursive then rules_of_deps rule.Rule.deps else Memo.return [])
  >>| function
  | Ok rules -> Anon_rules.Aliases.apply ~rules aliases
  | Error cycle ->
    User_error.raise [ Pp.text "Dependency cycle detected:"; Pp.chain cycle ~f:Rule.pp ]
;;
