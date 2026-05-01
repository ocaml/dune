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

      let repr =
        Repr.record
          "rule-cache-workspace-local-entry"
          [ Repr.field "rule_digest" (Repr.abstract Digest.to_dyn) ~get:(fun t ->
              t.rule_digest)
          ; Repr.field
              "dynamic_deps_stages"
              (Repr.list
                 (Repr.pair (Repr.abstract Dep.Set.to_dyn) (Repr.abstract Digest.to_dyn)))
              ~get:(fun t -> t.dynamic_deps_stages)
          ; Repr.field "targets_digest" (Repr.abstract Digest.to_dyn) ~get:(fun t ->
              t.targets_digest)
          ]
      ;;

      let to_dyn = Repr.to_dyn repr
    end

    type digest =
      { digest : Digest.t
      ; siblings : Digest.t Targets.Produced.t
      ; generation : int
      }

    let digest_repr =
      Repr.record
        "rule-cache-workspace-local-digest"
        [ Repr.field "digest" (Repr.abstract Digest.to_dyn) ~get:(fun t -> t.digest)
        ; Repr.field "siblings" (Repr.abstract Targets.Produced.to_dyn) ~get:(fun t ->
            t.siblings)
        ; Repr.field "generation" Repr.int ~get:(fun t -> t.generation)
        ]
    ;;

    let dyn_of_digest = Repr.to_dyn digest_repr

    (* Keyed by the first target of the rule. *)
    type t =
      { rules : Entry.t Path.Table.t
      ; digests : digest Path.Build.Table.t
      ; invalidated_subtrees : int Path.Build.Table.t
        (* A digest is only valid if its generation is greater or equal to the
           generation of all of its parents *)
      ; mutable generation : int (* The current generation *)
      }

    let file = Path.relative Path.build_dir ".db"

    let repr =
      Repr.record
        "rule-cache-workspace-local-database"
        [ Repr.field
            "rules"
            (Repr.abstract (Path.Table.to_dyn Entry.to_dyn))
            ~get:(fun t -> t.rules)
        ; Repr.field
            "digests"
            (Repr.abstract (Path.Build.Table.to_dyn dyn_of_digest))
            ~get:(fun t -> t.digests)
        ; Repr.field
            "invalidated_subtrees"
            (Repr.abstract (Path.Build.Table.to_dyn Dyn.int))
            ~get:(fun t -> t.invalidated_subtrees)
        ; Repr.field "generation" Repr.int ~get:(fun t -> t.generation)
        ]
    ;;

    let to_dyn = Repr.to_dyn repr

    module P = Dune_util.Persistent.Make (struct
        type nonrec t = t

        let name = "INCREMENTAL-DB"
        let version = 8
        let sharing = true
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
         | None ->
           { rules = Path.Table.create ()
           ; digests = Path.Build.Table.create 128
           ; invalidated_subtrees = Path.Build.Table.create 16
           ; generation = 0
           })
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
    let () = At_exit.at_exit_ignore Dune_trace.at_exit dump

    let get path =
      let t = Lazy.force t in
      Path.Table.find t.rules path
    ;;

    let set path e (targets : _ Targets.Produced.t) =
      let t = Lazy.force t in
      needs_dumping := true;
      Path.Table.set t.rules path e;
      let set_digest p digest =
        let digest = { digest; siblings = targets; generation = t.generation } in
        Path.Build.Table.set t.digests (Path.Build.append_local targets.root p) digest
      in
      Targets.Produced.iteri targets ~f:set_digest ~d:(fun _ -> ())
    ;;

    let remove (targets : Targets.Validated.t) =
      let t = Lazy.force t in
      needs_dumping := true;
      let remove = Path.Build.Table.remove t.digests in
      Targets.Validated.iter targets ~file:remove ~dir:remove
    ;;

    let remove_target path =
      let t = Lazy.force t in
      needs_dumping := true;
      match Path.Build.Table.find t.digests path with
      | None -> ()
      | Some { digest = _; siblings; _ } ->
        let head = Targets.Produced.head siblings in
        Path.Table.remove t.rules (Path.build head);
        Targets.Produced.iter_files siblings ~f:(fun path (_ : Digest.t) ->
          let path = Path.Build.append_local siblings.root path in
          Path.Build.Table.remove t.digests path)
    ;;

    let digest =
      (* We don't need to look up all the parents. Finding one greater should be enough
         to invalidate *)
      let invalidation_generation t path =
        let rec loop path acc =
          let acc =
            match Path.Build.Table.find t.invalidated_subtrees path with
            | None -> acc
            | Some generation -> Int.max acc generation
          in
          match Path.Build.parent path with
          | None -> acc
          | Some path -> loop path acc
        in
        loop path 0
      in
      fun path ->
        let t = Lazy.force t in
        match Path.Build.Table.find t.digests path with
        | None -> None
        | Some ({ generation; _ } as digest) ->
          if generation >= invalidation_generation t path
          then Some digest
          else (
            remove_target path;
            None)
    ;;

    let remove_subtree root =
      let t = Lazy.force t in
      needs_dumping := true;
      t.generation <- t.generation + 1;
      Path.Build.Table.set t.invalidated_subtrees root t.generation
    ;;
  end

  let store ~targets ~head_target ~rule_digest ~dynamic_deps_stages ~targets_digest =
    Database.set
      (Path.build head_target)
      { rule_digest; dynamic_deps_stages; targets_digest }
      targets
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

    let to_string reason =
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
    ;;
  end

  let compute_target_digests (targets : Targets.Validated.t)
    : (Digest.t Targets.Produced.t, Miss_reason.t) Dune_cache.Hit_or_miss.t
    =
    match Targets.Produced.of_validated targets with
    | Error error -> Miss (Miss_reason.Error_while_collecting_directory_targets error)
    | Ok targets ->
      (match
         Targets.Produced.map_with_errors targets ~f:(fun file ->
           match Database.digest file with
           | None -> Error ()
           | Some { digest; siblings = _; _ } -> Ok digest)
       with
       | Ok produced_targets -> Dune_cache.Hit_or_miss.Hit produced_targets
       | Error _ -> Miss Miss_reason.Targets_missing)
  ;;

  let lookup_impl ~rule_digest ~targets ~env ~build_deps =
    let prev_trace_with_produced_targets =
      match
        (* will be [None] if [head_target] was never built before. *)
        let head_target = Targets.Validated.head targets in
        Database.get (Path.build head_target)
      with
      | None -> Miss Miss_reason.No_previous_record
      | Some prev_trace ->
        (match Digest.equal prev_trace.rule_digest rule_digest with
         | false -> Miss (Miss_reason.Rule_changed (prev_trace.rule_digest, rule_digest))
         | true ->
           (* [compute_target_digests] returns a [Miss] if not all targets are
              available in the workspace-local cache. *)
           (match compute_target_digests targets with
            | Miss reason -> Miss reason
            | Hit produced_targets ->
              if
                Digest.equal
                  prev_trace.targets_digest
                  (Targets.Produced.digest produced_targets)
              then Hit (prev_trace, produced_targets)
              else Miss Targets_changed))
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
          let new_digest =
            let d = Digest.Manual.create () in
            Dep.Facts.digest deps d ~env;
            Digest.Manual.get d
          in
          if Digest.equal old_digest new_digest
          then loop rest
          else Fiber.return (Miss Miss_reason.Dynamic_deps_changed)
      in
      loop prev_trace.dynamic_deps_stages
  ;;

  let lookup ~always_rerun ~rule_digest ~targets ~env ~build_deps
    : Digest.t Targets.Produced.t option Fiber.t
    =
    let open Fiber.O in
    (if always_rerun
     then Fiber.return (Miss Miss_reason.Always_rerun)
     else lookup_impl ~rule_digest ~targets ~env ~build_deps)
    >>| function
    | Hit result -> Some result
    | Miss reason ->
      let always_emit =
        match reason with
        | Miss_reason.Error_while_collecting_directory_targets _ -> true
        | _ -> false
      in
      let event () =
        let reason = Miss_reason.to_string reason in
        let head_target = Targets.Validated.head targets in
        Dune_trace.Event.Cache.workspace_local_miss ~head:head_target ~reason
      in
      if always_emit
      then Dune_trace.always_emit (event ())
      else Dune_trace.emit ~buffered:true Cache (fun () -> event ());
      None
  ;;

  let remove targets = Database.remove targets
  let remove_target = Database.remove_target
  let remove_subtree = Database.remove_subtree
end
