open Import

module type S = Shared_intf.S

let shared_cache_key_string_for_log ~rule_digest ~head_target =
  sprintf "[%s] (%s)" (Digest.to_string rule_digest) (Path.Build.to_string head_target)
;;

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
      | Rerunning_for_reproducibility_check -> "rerunning for reproducibility check"
      | Not_found_in_cache -> "not found in cache"
    in
    Console.print_user_message
      (User_message.make
         [ Pp.hbox
             (Pp.textf
                "Shared cache miss %s: %s"
                (shared_cache_key_string_for_log ~rule_digest ~head_target)
                reason)
         ])
  ;;
end

module Make (S : sig
    val debug_shared_cache : bool
    val config : Config.t
    val upload : rule_digest:Dune_digest.t -> unit Fiber.t
    val download : rule_digest:Dune_digest.t -> unit Fiber.t
  end) =
struct
  open S

  let try_to_restore_from_shared_cache ~mode ~rule_digest ~targets
    : (Digest.t Targets.Produced.t, Miss_reason.t) Hit_or_miss.t Fiber.t
    =
    let open Fiber.O in
    let+ () = download ~rule_digest in
    let key () =
      shared_cache_key_string_for_log
        ~rule_digest
        ~head_target:(Targets.Validated.head targets)
    in
    match Local.restore_artifacts ~mode ~rule_digest ~target_dir:targets.root with
    | Restored artifacts ->
      (* it's a small departure from the general "debug cache" semantics that
         we're also printing successes, but it can be useful to see successes
         too if the goal is to understand when and how the file in the build
         directory appeared *)
      if debug_shared_cache then Log.info [ Pp.textf "cache restore success %s" (key ()) ];
      Hit_or_miss.Hit artifacts
    | Not_found_in_cache -> Hit_or_miss.Miss Miss_reason.Not_found_in_cache
    | Error exn -> Miss (Error (Printexc.to_string exn))
  ;;

  let lookup_impl ~rule_digest ~targets =
    match config with
    | Disabled -> Fiber.return (Hit_or_miss.Miss Miss_reason.Cache_disabled)
    | Enabled { storage_mode = mode; reproducibility_check } ->
      (match Config.Reproducibility_check.sample reproducibility_check with
       | true ->
         (* CR-someday amokhov: Here we re-execute the rule, as in Jenga. To make
            [check_probability] more meaningful, we could first make sure that
            the shared cache actually does contain an entry for [rule_digest]. *)
         Fiber.return (Hit_or_miss.Miss Miss_reason.Rerunning_for_reproducibility_check)
       | false -> try_to_restore_from_shared_cache ~mode ~rule_digest ~targets)
  ;;

  let lookup ~can_go_in_shared_cache ~rule_digest ~targets
    : Digest.t Targets.Produced.t option Fiber.t
    =
    let open Fiber.O in
    let+ result =
      match can_go_in_shared_cache with
      | false -> Fiber.return (Hit_or_miss.Miss Miss_reason.Cannot_go_in_shared_cache)
      | true -> lookup_impl ~rule_digest ~targets
    in
    match result with
    | Hit result -> Some result
    | Miss reason ->
      (match debug_shared_cache, reason with
       | true, _ | false, Error _ ->
         (* Always log errors because they are not expected as a part of normal
            operation and might indicate a problem. *)
         Miss_reason.report
           reason
           ~rule_digest
           ~head_target:(Targets.Validated.head targets)
       | false, _ -> ());
      None
  ;;

  (* If this function fails to store the rule to the shared cache, it returns
     [None] because we don't want this to be a catastrophic error. We simply log
     this incident and continue without saving the rule to the shared cache. *)
  let try_to_store_to_shared_cache ~mode ~rule_digest ~action ~produced_targets
    : Digest.t Targets.Produced.t option Fiber.t
    =
    let open Fiber.O in
    let hex = Digest.to_string rule_digest in
    let pp_error msg =
      let action = action () in
      Pp.concat
        [ Pp.textf "cache store error [%s]: %s after executing" hex msg
        ; Pp.space
        ; Pp.char '('
        ; action
        ; Pp.char ')'
        ]
    in
    let update_cached_digests ~targets_and_digests =
      Targets.Produced.iteri targets_and_digests ~f:(fun path digest ->
        Cached_digest.set (Path.Build.append_local targets_and_digests.root path) digest)
    in
    match
      Targets.Produced.map_with_errors
        produced_targets
        ~all_errors:false
        ~f:(fun target () ->
          match Local.Target.create target with
          | Some t -> Ok t
          | None -> Error ())
    with
    | Error _ -> Fiber.return None
    | Ok targets ->
      let compute_digest ~executable path =
        Fiber.return (Digest.file_with_executable_bit ~executable path)
      in
      Local.store_artifacts ~mode ~rule_digest ~compute_digest targets
      >>= (function
       | Stored targets_and_digests ->
         let+ () = upload ~rule_digest in
         Log.info [ Pp.textf "cache store success [%s]" hex ];
         update_cached_digests ~targets_and_digests;
         Some targets_and_digests
       | Already_present targets_and_digests ->
         Log.info [ Pp.textf "cache store skipped [%s]: already present" hex ];
         update_cached_digests ~targets_and_digests;
         Fiber.return (Some targets_and_digests)
       | Error (Unix.Unix_error (Unix.EXDEV, "link", file)) ->
         (* We cannot hardlink across partitions so we kindly let the user know
            that they should use copy cache instead. *)
         Log.info
           [ Pp.concat
               [ Pp.textf "cache store error [%s]:" hex
               ; Pp.space
               ; Pp.textf
                   "cannot link %s between file systems. Use (cache-storage-mode copy) \
                    instead."
                   file
               ]
           ];
         Fiber.return None
       | Error exn ->
         Log.info [ pp_error (Printexc.to_string exn) ];
         Fiber.return None
       | Will_not_store_due_to_non_determinism sexp ->
         (* CR-someday amokhov: We should systematically log all warnings. *)
         Log.info [ pp_error (Sexp.to_string sexp) ];
         User_warning.emit [ pp_error (Sexp.to_string sexp) ];
         Fiber.return None)
  ;;

  let compute_target_digests_or_raise_error
    ~should_remove_write_permissions_on_generated_files
    ~loc
    ~produced_targets
    : Digest.t Targets.Produced.t
    =
    let compute_digest =
      (* Remove write permissions on targets. A first theoretical reason is that
         the build process should be a computational graph and targets should
         not change state once built. A very practical reason is that enabling
         the cache will remove write permission because of hardlink sharing
         anyway, so always removing them enables to catch mistakes earlier. *)
      Cached_digest.refresh
        ~allow_dirs:true
        ~remove_write_permissions:should_remove_write_permissions_on_generated_files
    in
    match
      Targets.Produced.map_with_errors
        produced_targets
        ~all_errors:true
        ~f:(fun target () -> compute_digest target)
    with
    | Ok result -> result
    | Error errors ->
      let missing, errors =
        let process_target (target, error) =
          match error with
          | Cached_digest.Digest_result.Error.No_such_file -> Left target
          | Broken_symlink ->
            let error = Pp.verbatim "Broken symbolic link" in
            Right (target, error)
          | Cyclic_symlink ->
            let error = Pp.verbatim "Cyclic symbolic link" in
            Right (target, error)
          | Unexpected_kind file_kind ->
            let error =
              Pp.verbatim
                (sprintf
                   "Unexpected file kind %S (%s)"
                   (File_kind.to_string file_kind)
                   (File_kind.to_string_hum file_kind))
            in
            Right (target, error)
          | Unix_error (error, syscall, arg) ->
            let unix_error = Unix_error.Detailed.create error ~syscall ~arg in
            Right (target, Unix_error.Detailed.pp unix_error)
          | Unrecognized exn ->
            let error =
              Pp.verbatim
                (match exn with
                 | Sys_error msg ->
                   let prefix =
                     let expected_syscall_path = Path.to_string (Path.build target) in
                     expected_syscall_path ^ ": "
                   in
                   String.drop_prefix_if_exists ~prefix msg
                 | exn -> Printexc.to_string exn)
            in
            Right (target, error)
        in
        Nonempty_list.to_list errors |> List.partition_map ~f:process_target
      in
      (match missing, errors with
       | [], [] ->
         (* This is impossible because [errors] is non-empty and [List.partition_map]
            on a non empty list should also return at least one least that isn't empty.
            Unfortunately, the type of such a [Nonempty_list.partition_map] would be
            rather awkward. *)
         Code_error.raise
           "compute_target_digests_or_raise_error: this is impossible because we should \
            at least be showing the original error"
           [ "targets", Targets.Produced.to_dyn produced_targets ]
       | missing, errors ->
         User_error.raise
           ~loc
           ((match missing with
             | [] -> []
             | _ ->
               [ Pp.textf "Rule failed to generate the following targets:"
               ; List.sort missing ~compare:Path.Build.compare
                 |> List.map ~f:Path.build
                 |> Pp.enumerate ~f:Path.pp
               ])
            @
            match errors with
            | [] -> []
            | _ ->
              [ Pp.textf "Error trying to read targets after a rule was run:"
              ; List.sort errors ~compare:(fun x y -> Path.Build.compare (fst x) (fst y))
                |> Pp.enumerate ~f:(fun (target, error) ->
                  Pp.concat ~sep:(Pp.verbatim ": ") [ Path.pp (Path.build target); error ])
              ]))
  ;;

  let examine_targets_and_store
    ~can_go_in_shared_cache
    ~loc
    ~rule_digest
    ~should_remove_write_permissions_on_generated_files
    ~action
    ~(produced_targets : unit Targets.Produced.t)
    : Digest.t Targets.Produced.t Fiber.t
    =
    match config with
    | Enabled { storage_mode = mode; reproducibility_check = _ }
      when can_go_in_shared_cache ->
      let open Fiber.O in
      let+ produced_targets_with_digests =
        try_to_store_to_shared_cache ~mode ~rule_digest ~produced_targets ~action
      in
      (match produced_targets_with_digests with
       | Some produced_targets_with_digests -> produced_targets_with_digests
       | None ->
         compute_target_digests_or_raise_error
           ~should_remove_write_permissions_on_generated_files
           ~loc
           ~produced_targets)
    | _ ->
      Fiber.return
        (compute_target_digests_or_raise_error
           ~should_remove_write_permissions_on_generated_files
           ~loc
           ~produced_targets)
  ;;
end
