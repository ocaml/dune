open Import
open Dune_cache.Hit_or_miss

module Workspace_local = struct
  (* Stores information for deciding if a rule needs to be re-executed. *)
  module Database = struct
    type digest = Workspace_cache.Rule_cache.digest

    (* Keyed by the first target of the rule. *)
    type t = Workspace_cache.Rule_cache.t =
      { rules : Workspace_cache.Rule_cache.Entry.t Path.Table.t
      ; digests : digest Path.Build.Table.t
      ; invalidated_subtrees : int Path.Build.Table.t
        (* A digest is only valid if its generation is greater or equal to the
           generation of all of its parents *)
      ; mutable generation : int (* The current generation *)
      }

    let t = lazy (Workspace_cache.rule_cache ())

    let get path =
      let t = Lazy.force t in
      Path.Table.find t.rules path
    ;;

    let set path e (targets : _ Targets.Produced.t) =
      let t = Lazy.force t in
      Workspace_cache.mark_dirty ();
      Path.Table.set t.rules path e;
      let set_digest p digest =
        let digest : digest = { digest; siblings = targets; generation = t.generation } in
        Path.Build.Table.set t.digests (Path.Build.append_local targets.root p) digest
      in
      Targets.Produced.iteri targets ~f:set_digest ~d:(fun _ -> ())
    ;;

    let remove (targets : Targets.Validated.t) =
      let t = Lazy.force t in
      Workspace_cache.mark_dirty ();
      let remove = Path.Build.Table.remove t.digests in
      Targets.Validated.iter targets ~file:remove ~dir:remove
    ;;

    let remove_target path =
      let t = Lazy.force t in
      Workspace_cache.mark_dirty ();
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
      Workspace_cache.mark_dirty ();
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
