open Import
open Dune_cache.Hit_or_miss

module Workspace_local = struct
  (* Stores information for deciding if a rule needs to be re-executed. *)
  module Database = struct
    module Entry = struct
      type t =
        { rule_digest : Digest.t
        ; dynamic_deps_stages : (Dep.Set.t * Digest.t) list
        ; targets_digest : Digest.t
        }

      let to_dyn { rule_digest; dynamic_deps_stages; targets_digest } =
        Dyn.Record
          [ "rule_digest", Digest.to_dyn rule_digest
          ; ( "dynamic_deps_stages"
            , Dyn.list (Dyn.pair Dep.Set.to_dyn Digest.to_dyn) dynamic_deps_stages )
          ; "targets_digest", Digest.to_dyn targets_digest
          ]
      ;;
    end

    (* Keyed by the first target of the rule. *)
    type t = Entry.t Path.Table.t

    let file = Path.relative Path.build_dir ".db"
    let to_dyn = Path.Table.to_dyn Entry.to_dyn

    module P = Dune_util.Persistent.Make (struct
        type nonrec t = t

        let name = "INCREMENTAL-DB"
        let version = 5
        let to_dyn = to_dyn

        let test_example () =
          let table = Path.Table.create () in
          Path.Table.set
            table
            (Path.external_ (Path.External.of_string "/"))
            { Entry.rule_digest = Digest.string "foo"
            ; dynamic_deps_stages = [ Dep.Set.empty, Digest.string "bar" ]
            ; targets_digest = Digest.string "zzz"
            };
          table
        ;;
      end)

    let needs_dumping = ref false

    let t =
      lazy
        (match P.load file with
         | Some t -> t
         (* This mutable table is safe: it's only used by [execute_rule_impl] to
            decide whether to rebuild a rule or not; [execute_rule_impl] ensures
            that the targets are produced deterministically. *)
         | None -> Path.Table.create ())
    ;;

    let dump () =
      if !needs_dumping && Path.build_dir_exists ()
      then (
        needs_dumping := false;
        Console.Status_line.with_overlay
          (Live (fun () -> Pp.hbox (Pp.text "Saving build trace db...")))
          ~f:(fun () -> P.dump file (Lazy.force t)))
    ;;

    (* CR-someday amokhov: If this happens to be executed after we've cleared
       the status line and printed some text afterwards, [dump] would overwrite
       that text by the "Saving..." message. If this hypothetical scenario turns
       out to be a real problem, we will need to add some synchronisation
       mechanism to prevent clearing the status line too early. *)
    let () = at_exit dump

    let get path =
      let t = Lazy.force t in
      Path.Table.find t path
    ;;

    let set path e =
      let t = Lazy.force t in
      needs_dumping := true;
      Path.Table.set t path e
    ;;
  end

  let store ~head_target ~rule_digest ~dynamic_deps_stages ~targets_digest =
    Database.set
      (Path.build head_target)
      { rule_digest; dynamic_deps_stages; targets_digest }
  ;;

  module Miss_reason = struct
    type t =
      | No_previous_record
      | Rule_changed of Digest.t * Digest.t
      | Targets_changed
      | Targets_missing
      | Dynamic_deps_changed
      | Always_rerun
      | Error_while_collecting_directory_targets of Targets.Produced.Error.t

    let report ~head_target reason =
      let reason =
        match reason with
        | No_previous_record -> "never seen this target before"
        | Rule_changed (before, after) ->
          sprintf
            "rule or dependencies changed: %s -> %s"
            (Digest.to_string before)
            (Digest.to_string after)
        | Targets_missing -> "target missing from build dir"
        | Targets_changed -> "target changed in build dir"
        | Always_rerun -> "not trying to use the cache"
        | Dynamic_deps_changed -> "dynamic dependencies changed"
        | Error_while_collecting_directory_targets error ->
          sprintf
            "error while collecting directory targets: %s"
            (Targets.Produced.Error.to_string_hum error)
      in
      Console.print_user_message
        (User_message.make
           [ Pp.hbox
               (Pp.textf
                  "Workspace-local cache miss: %s: %s"
                  (Path.Build.to_string head_target)
                  reason)
           ])
    ;;
  end

  let compute_target_digests (targets : Targets.Validated.t)
    : (Digest.t Targets.Produced.t, Miss_reason.t) Dune_cache.Hit_or_miss.t
    =
    match Targets.Produced.of_validated targets with
    | Error error -> Miss (Error_while_collecting_directory_targets error)
    | Ok targets ->
      (match
         Targets.Produced.map_with_errors targets ~all_errors:false ~f:(fun target () ->
           Cached_digest.build_file ~allow_dirs:true target)
       with
       | Ok produced_targets -> Dune_cache.Hit_or_miss.Hit produced_targets
       | Error _ -> Miss Targets_missing)
  ;;

  let lookup_impl ~rule_digest ~targets ~env ~build_deps =
    (* [prev_trace] will be [None] if [head_target] was never built before. *)
    let head_target = Targets.Validated.head targets in
    let prev_trace = Database.get (Path.build head_target) in
    let prev_trace_with_produced_targets =
      match prev_trace with
      | None -> Miss Miss_reason.No_previous_record
      | Some prev_trace ->
        (match Digest.equal prev_trace.rule_digest rule_digest with
         | false -> Miss (Rule_changed (prev_trace.rule_digest, rule_digest))
         | true ->
           (* [compute_target_digests] returns a [Miss] if not all targets are
              available in the workspace-local cache. *)
           (match compute_target_digests targets with
            | Miss reason -> Miss reason
            | Hit produced_targets ->
              (match
                 Digest.equal
                   prev_trace.targets_digest
                   (Targets.Produced.digest produced_targets)
               with
               | true -> Hit (prev_trace, produced_targets)
               | false -> Miss Targets_changed)))
    in
    match prev_trace_with_produced_targets with
    | Miss reason -> Fiber.return (Miss reason)
    | Hit (prev_trace, produced_targets) ->
      (* CR-someday aalekseyev: If there's a change at one of the last stages,
         we still re-run all the previous stages, which is a bit of a waste. We
         could remember what stage needs re-running and only re-run that (and
         later stages). *)
      let rec loop stages =
        match stages with
        | [] -> Fiber.return (Hit produced_targets)
        | (deps, old_digest) :: rest ->
          let open Fiber.O in
          let* deps = Memo.run (build_deps deps) in
          let new_digest = Dep.Facts.digest deps ~env in
          (match Digest.equal old_digest new_digest with
           | true -> loop rest
           | false -> Fiber.return (Miss Miss_reason.Dynamic_deps_changed))
      in
      loop prev_trace.dynamic_deps_stages
  ;;

  let lookup ~always_rerun ~rule_digest ~targets ~env ~build_deps
    : Digest.t Targets.Produced.t option Fiber.t
    =
    let open Fiber.O in
    let+ result =
      match always_rerun with
      | true -> Fiber.return (Miss Miss_reason.Always_rerun)
      | false -> lookup_impl ~rule_digest ~targets ~env ~build_deps
    in
    match result with
    | Hit result -> Some result
    | Miss reason ->
      let t = Build_config.get () in
      if t.cache_debug_flags.workspace_local_cache
      then Miss_reason.report reason ~head_target:(Targets.Validated.head targets);
      None
  ;;
end

module Shared = struct
  let lookup ~can_go_in_shared_cache ~rule_digest ~targets =
    let config = Build_config.get () in
    let module Shared_cache = (val config.shared_cache) in
    Shared_cache.lookup ~can_go_in_shared_cache ~rule_digest ~targets
  ;;

  let examine_targets_and_store
    ~can_go_in_shared_cache
    ~loc
    ~rule_digest
    ~should_remove_write_permissions_on_generated_files
    ~action
    ~produced_targets
    =
    let config = Build_config.get () in
    let module Shared_cache = (val config.shared_cache) in
    Shared_cache.examine_targets_and_store
      ~can_go_in_shared_cache
      ~loc
      ~rule_digest
      ~should_remove_write_permissions_on_generated_files
      ~action
      ~produced_targets
  ;;
end
