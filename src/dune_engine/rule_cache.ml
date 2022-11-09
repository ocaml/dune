open Import

(* A type isomorphic to [Result], but without the negative connotations
   associated with the word "error". *)
module Result = struct
  type ('hit, 'miss) t =
    | Hit of 'hit
    | Miss of 'miss
end

module Workspace_local = struct
  (* Stores information for deciding if a rule needs to be re-executed. *)
  module Database = struct
    module Entry = struct
      type t =
        { rule_digest : Digest.t
        ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
        ; targets_digest : Digest.t
        }

      let to_dyn { rule_digest; dynamic_deps_stages; targets_digest } =
        Dyn.Record
          [ ("rule_digest", Digest.to_dyn rule_digest)
          ; ( "dynamic_deps_stages"
            , Dyn.list
                (Dyn.pair Action_exec.Dynamic_dep.Set.to_dyn Digest.to_dyn)
                dynamic_deps_stages )
          ; ("targets_digest", Digest.to_dyn targets_digest)
          ]
    end

    (* Keyed by the first target of the rule. *)
    type t = Entry.t Path.Table.t

    let file = Path.relative Path.build_dir ".db"

    let to_dyn = Path.Table.to_dyn Entry.to_dyn

    module P = Dune_util.Persistent.Make (struct
      type nonrec t = t

      let name = "INCREMENTAL-DB"

      let version = 4

      let to_dyn = to_dyn
    end)

    let needs_dumping = ref false

    let t =
      lazy
        (match P.load file with
        | Some t -> t
        (* This mutable table is safe: it's only used by [execute_rule_impl] to
           decide whether to rebuild a rule or not; [execute_rule_impl] ensures
           that the targets are produced deterministically. *)
        | None -> Path.Table.create 1024)

    let dump () =
      if !needs_dumping && Path.build_dir_exists () then (
        needs_dumping := false;
        Console.Status_line.with_overlay
          (Live (fun () -> Pp.hbox (Pp.text "Saving build trace db...")))
          ~f:(fun () -> P.dump file (Lazy.force t)))

    (* CR-someday amokhov: If this happens to be executed after we've cleared
       the status line and printed some text afterwards, [dump] would overwrite
       that text by the "Saving..." message. If this hypothetical scenario turns
       out to be a real problem, we will need to add some synchronisation
       mechanism to prevent clearing the status line too early. *)
    let () = at_exit dump

    let get path =
      let t = Lazy.force t in
      Path.Table.find t path

    let set path e =
      let t = Lazy.force t in
      needs_dumping := true;
      Path.Table.set t path e
  end

  let store ~head_target ~rule_digest ~dynamic_deps_stages ~targets_digest =
    Database.set (Path.build head_target)
      { rule_digest; dynamic_deps_stages; targets_digest }

  module Miss_reason = struct
    type t =
      | No_previous_record
      | Rule_changed of Digest.t * Digest.t
      | Targets_changed
      | Targets_missing
      | Dynamic_deps_changed
      | Always_rerun
      | Error_while_collecting_directory_targets of Unix_error.Detailed.t

    let report ~head_target reason =
      let reason =
        match reason with
        | No_previous_record -> "never seen this target before"
        | Rule_changed (before, after) ->
          sprintf "rule or dependencies changed: %s -> %s"
            (Digest.to_string before) (Digest.to_string after)
        | Targets_missing -> "target missing from build dir"
        | Targets_changed -> "target changed in build dir"
        | Always_rerun -> "not trying to use the cache"
        | Dynamic_deps_changed -> "dynamic dependencies changed"
        | Error_while_collecting_directory_targets unix_error ->
          sprintf "error while collecting directory targets: %s"
            (Unix_error.Detailed.to_string_hum unix_error)
      in
      Console.print_user_message
        (User_message.make
           [ Pp.hbox
               (Pp.textf "Workspace-local cache miss: %s: %s"
                  (Path.Build.to_string head_target)
                  reason)
           ])
  end

  let compute_target_digests (targets : Targets.Validated.t) :
      (Digest.t Targets.Produced.t, Miss_reason.t) Result.t =
    match Targets.Produced.of_validated targets with
    | Error (_, unix_error) ->
      Miss (Error_while_collecting_directory_targets unix_error)
    | Ok targets -> (
      match
        Targets.Produced.Option.mapi targets ~f:(fun target () ->
            Cached_digest.build_file ~allow_dirs:true target
            |> Cached_digest.Digest_result.to_option)
      with
      | Some produced_targets -> Hit produced_targets
      | None -> Miss Targets_missing)

  let lookup_impl ~rule_digest ~targets ~env ~build_deps =
    (* [prev_trace] will be [None] if [head_target] was never built before. *)
    let head_target = Targets.Validated.head targets in
    let prev_trace = Database.get (Path.build head_target) in
    let prev_trace_with_produced_targets =
      match prev_trace with
      | None -> Result.Miss Miss_reason.No_previous_record
      | Some prev_trace -> (
        match Digest.equal prev_trace.rule_digest rule_digest with
        | false -> Miss (Rule_changed (prev_trace.rule_digest, rule_digest))
        | true -> (
          (* [compute_target_digests] returns a [Miss] if not all targets are
             available in the workspace-local cache. *)
          match compute_target_digests targets with
          | Miss reason -> Miss reason
          | Hit produced_targets -> (
            match
              Digest.equal prev_trace.targets_digest
                (Targets.Produced.digest produced_targets)
            with
            | true -> Hit (prev_trace, produced_targets)
            | false -> Miss Targets_changed)))
    in
    match prev_trace_with_produced_targets with
    | Result.Miss reason -> Fiber.return (Result.Miss reason)
    | Hit (prev_trace, produced_targets) ->
      (* CR-someday aalekseyev: If there's a change at one of the last stages,
         we still re-run all the previous stages, which is a bit of a waste. We
         could remember what stage needs re-running and only re-run that (and
         later stages). *)
      let rec loop stages =
        match stages with
        | [] -> Fiber.return (Result.Hit produced_targets)
        | (deps, old_digest) :: rest -> (
          let deps = Action_exec.Dynamic_dep.Set.to_dep_set deps in
          let open Fiber.O in
          let* deps = Memo.run (build_deps deps) in
          let new_digest = Dep.Facts.digest deps ~env in
          match Digest.equal old_digest new_digest with
          | true -> loop rest
          | false -> Fiber.return (Result.Miss Miss_reason.Dynamic_deps_changed)
          )
      in
      loop prev_trace.dynamic_deps_stages

  let lookup ~always_rerun ~rule_digest ~targets ~env ~build_deps :
      Digest.t Targets.Produced.t option Fiber.t =
    let open Fiber.O in
    let+ result =
      match always_rerun with
      | true -> Fiber.return (Result.Miss Miss_reason.Always_rerun)
      | false -> lookup_impl ~rule_digest ~targets ~env ~build_deps
    in
    match result with
    | Hit result -> Some result
    | Miss reason ->
      let t = Build_config.get () in
      if t.cache_debug_flags.workspace_local_cache then
        Miss_reason.report reason ~head_target:(Targets.Validated.head targets);
      None
end

module Shared = struct
  let shared_cache_key_string_for_log ~rule_digest ~head_target =
    sprintf "[%s] (%s)"
      (Digest.to_string rule_digest)
      (Path.Build.to_string head_target)

  module Miss_reason = struct
    type t =
      | Cache_disabled
      | Cannot_go_in_shared_cache
      | Rerunning_for_reproducibility_check
      | Not_found_in_cache
      | Error of string

    let report ~rule_digest ~head_target reason =
      let reason =
        match reason with
        | Cache_disabled -> "cache disabled"
        | Cannot_go_in_shared_cache -> "can't go in shared cache"
        | Error exn -> sprintf "error: %s" exn
        | Rerunning_for_reproducibility_check ->
          "rerunning for reproducibility check"
        | Not_found_in_cache -> "not found in cache"
      in
      Console.print_user_message
        (User_message.make
           [ Pp.hbox
               (Pp.textf "Shared cache miss %s: %s"
                  (shared_cache_key_string_for_log ~rule_digest ~head_target)
                  reason)
           ])
  end

  (* CR-someday amokhov: If the cloud cache is enabled, then before attempting
     to restore artifacts from the shared cache, we should send a download
     request for [rule_digest] to the cloud. *)
  let try_to_restore_from_shared_cache ~mode ~rule_digest ~head_target
      ~target_dir : (Digest.t Targets.Produced.t, Miss_reason.t) Result.t =
    let key () = shared_cache_key_string_for_log ~rule_digest ~head_target in
    match Dune_cache.Local.restore_artifacts ~mode ~rule_digest ~target_dir with
    | Restored res ->
      (* it's a small departure from the general "debug cache" semantics that
         we're also printing successes, but it can be useful to see successes
         too if the goal is to understand when and how the file in the build
         directory appeared *)
      let t = Build_config.get () in
      if t.cache_debug_flags.shared_cache then
        Log.info [ Pp.textf "cache restore success %s" (key ()) ];
      Hit (Targets.Produced.of_file_list_exn res)
    | Not_found_in_cache -> Miss Not_found_in_cache
    | Error exn -> Miss (Error (Printexc.to_string exn))

  let lookup_impl ~rule_digest ~targets ~target_dir =
    let t = Build_config.get () in
    match t.cache_config with
    | Disabled -> Result.Miss Miss_reason.Cache_disabled
    | Enabled { storage_mode = mode; reproducibility_check } -> (
      match
        Dune_cache.Config.Reproducibility_check.sample reproducibility_check
      with
      | true ->
        (* CR-someday amokhov: Here we re-execute the rule, as in Jenga. To make
           [check_probability] more meaningful, we could first make sure that
           the shared cache actually does contain an entry for [rule_digest]. *)
        Miss Rerunning_for_reproducibility_check
      | false ->
        try_to_restore_from_shared_cache ~mode
          ~head_target:(Targets.Validated.head targets)
          ~rule_digest ~target_dir)

  let lookup ~can_go_in_shared_cache ~rule_digest ~targets ~target_dir :
      Digest.t Targets.Produced.t option =
    let result =
      match can_go_in_shared_cache with
      | false -> Result.Miss Miss_reason.Cannot_go_in_shared_cache
      | true -> lookup_impl ~rule_digest ~targets ~target_dir
    in
    match result with
    | Hit result -> Some result
    | Miss reason ->
      let t = Build_config.get () in
      (match (t.cache_debug_flags.shared_cache, reason) with
      | true, _ | false, Error _ ->
        (* Always log errors because they are not expected as a part of normal
           operation and might indicate a problem. *)
        Miss_reason.report reason ~rule_digest
          ~head_target:(Targets.Validated.head targets)
      | false, _ -> ());
      None

  (* If this function fails to store the rule to the shared cache, it returns
     [None] because we don't want this to be a catastrophic error. We simply log
     this incident and continue without saving the rule to the shared cache. *)
  let try_to_store_to_shared_cache ~mode ~rule_digest ~action ~file_targets :
      Digest.t Targets.Produced.t option Fiber.t =
    let open Fiber.O in
    let hex = Digest.to_string rule_digest in
    let pp_error msg =
      let action = Action.for_shell action |> Action_to_sh.pp in
      Pp.concat
        [ Pp.textf "cache store error [%s]: %s after executing" hex msg
        ; Pp.space
        ; Pp.char '('
        ; action
        ; Pp.char ')'
        ]
    in
    let update_cached_digests ~targets_and_digests =
      List.iter targets_and_digests ~f:(fun (target, digest) ->
          Cached_digest.set target digest);
      Some (Targets.Produced.of_file_list_exn targets_and_digests)
    in
    match
      Path.Build.Map.to_list_map file_targets ~f:(fun target () ->
          Dune_cache.Local.Target.create target)
      |> Option.List.all
    with
    | None -> Fiber.return None
    | Some targets -> (
      let compute_digest ~executable path =
        Stdune.Result.try_with (fun () ->
            Digest.file_with_executable_bit ~executable path)
        |> Fiber.return
      in
      Dune_cache.Local.store_artifacts ~mode ~rule_digest ~compute_digest
        targets
      >>| function
      | Stored targets_and_digests ->
        (* CR-someday amokhov: Here and in the case below we can inform the
           cloud daemon that a new cache entry can be uploaded to the cloud. *)
        Log.info [ Pp.textf "cache store success [%s]" hex ];
        update_cached_digests ~targets_and_digests
      | Already_present targets_and_digests ->
        Log.info [ Pp.textf "cache store skipped [%s]: already present" hex ];
        update_cached_digests ~targets_and_digests
      | Error (Unix.Unix_error (Unix.EXDEV, "link", file)) ->
        (* We cannot hardlink across partitions so we kindly let the user know
           that they should use copy cache instead. *)
        Log.info
          [ Pp.concat
              [ Pp.textf "cache store error [%s]:" hex
              ; Pp.space
              ; Pp.textf
                  "cannot link %s between file systems. Use \
                   (cache-storage-mode copy) instead."
                  file
              ]
          ];
        None
      | Error exn ->
        Log.info [ pp_error (Printexc.to_string exn) ];
        None
      | Will_not_store_due_to_non_determinism sexp ->
        (* CR-someday amokhov: We should systematically log all warnings. *)
        Log.info [ pp_error (Sexp.to_string sexp) ];
        User_warning.emit [ pp_error (Sexp.to_string sexp) ];
        None)

  let compute_target_digests_or_raise_error exec_params ~loc ~produced_targets :
      Digest.t Targets.Produced.t =
    let compute_digest =
      (* Remove write permissions on targets. A first theoretical reason is that
         the build process should be a computational graph and targets should
         not change state once built. A very practical reason is that enabling
         the cache will remove write permission because of hardlink sharing
         anyway, so always removing them enables to catch mistakes earlier. *)
      (* FIXME: searching the dune version for each single target seems way
         suboptimal. This information could probably be stored in rules
         directly. *)
      let remove_write_permissions =
        Execution_parameters.should_remove_write_permissions_on_generated_files
          exec_params
      in
      Cached_digest.refresh ~allow_dirs:true ~remove_write_permissions
    in
    match
      Targets.Produced.Option.mapi produced_targets ~f:(fun target () ->
          compute_digest target |> Cached_digest.Digest_result.to_option)
    with
    | Some result -> result
    | None -> (
      let missing, errors =
        let process_target target (missing, errors) =
          match compute_digest target with
          | Ok (_ : Digest.t) -> (missing, errors)
          | No_such_file -> (target :: missing, errors)
          | Broken_symlink ->
            let error = Pp.verbatim "Broken symbolic link" in
            (missing, (target, error) :: errors)
          | Cyclic_symlink ->
            let error = Pp.verbatim "Cyclic symbolic link" in
            (missing, (target, error) :: errors)
          | Unexpected_kind file_kind ->
            let error =
              Pp.verbatim
                (sprintf "Unexpected file kind %S (%s)"
                   (File_kind.to_string file_kind)
                   (File_kind.to_string_hum file_kind))
            in
            (missing, (target, error) :: errors)
          | Unix_error (error, syscall, arg) ->
            let unix_error = Unix_error.Detailed.create error ~syscall ~arg in
            (missing, (target, Unix_error.Detailed.pp unix_error) :: errors)
          | Error exn ->
            let error =
              Pp.verbatim
                (match exn with
                | Sys_error msg ->
                  let prefix =
                    let expected_syscall_path =
                      Path.to_string (Path.build target)
                    in
                    expected_syscall_path ^ ": "
                  in
                  String.drop_prefix_if_exists ~prefix msg
                | exn -> Printexc.to_string exn)
            in
            (missing, (target, error) :: errors)
        in
        Path.Build.Map.foldi (Targets.Produced.all_files produced_targets)
          ~init:([], []) ~f:(fun target () -> process_target target)
      in
      match (missing, errors) with
      | [], [] ->
        Code_error.raise
          "compute_target_digests_or_raise_error: spurious target digest \
           failure"
          [ ("targets", Targets.Produced.to_dyn produced_targets) ]
      | missing, errors ->
        User_error.raise ~loc
          ((match missing with
           | [] -> []
           | _ ->
             [ Pp.textf "Rule failed to generate the following targets:"
             ; Pp.enumerate ~f:Path.pp (List.rev_map ~f:Path.build missing)
             ])
          @
          match errors with
          | [] -> []
          | _ ->
            [ Pp.textf "Error trying to read targets after a rule was run:"
            ; Pp.enumerate (List.rev errors) ~f:(fun (target, error) ->
                  Pp.concat ~sep:(Pp.verbatim ": ")
                    [ Path.pp (Path.build target); error ])
            ]))

  let examine_targets_and_store ~can_go_in_shared_cache ~loc ~rule_digest
      ~execution_parameters ~action
      ~(produced_targets : unit Targets.Produced.t) :
      Digest.t Targets.Produced.t Fiber.t =
    let t = Build_config.get () in
    match t.cache_config with
    | Enabled { storage_mode = mode; reproducibility_check = _ }
      when can_go_in_shared_cache -> (
      let open Fiber.O in
      let+ produced_targets_with_digests =
        try_to_store_to_shared_cache ~mode ~rule_digest
          ~file_targets:produced_targets.files ~action
      in
      match produced_targets_with_digests with
      | Some produced_targets_with_digests -> produced_targets_with_digests
      | None ->
        compute_target_digests_or_raise_error execution_parameters ~loc
          ~produced_targets)
    | _ ->
      Fiber.return
        (compute_target_digests_or_raise_error execution_parameters ~loc
           ~produced_targets)
end
