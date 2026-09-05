open Import
open Dune_cache.Hit_or_miss

module Dynamic = struct
  open Fiber.O

  let conv =
    let open Conv in
    let build_path =
      iso
        string
        (fun path -> Path.Build.of_local (Path.Local.of_string path))
        (fun path -> Path.Local.to_string (Path.Build.local path))
    in
    let path =
      let build = constr "build" build_path Path.build in
      let source =
        constr
          "source"
          (iso string Path.Source.of_string Path.Source.to_string)
          Path.source
      in
      let external_ =
        constr
          "external"
          (iso string Path.External.of_string Path.External.to_string)
          Path.external_
      in
      sum
        [ econstr build; econstr source; econstr external_ ]
        (function
          | Path.In_build_dir path -> case path build
          | In_source_tree path -> case path source
          | External path -> case path external_)
    in
    let selector =
      iso
        (triple path (enum [ "true", true; "false", false ]) Predicate_lang.Glob.conv)
        (fun (dir, only_generated_files, predicate) ->
           File_selector.of_predicate_lang ~dir ~only_generated_files predicate)
        (fun selector ->
           ( File_selector.dir selector
           , File_selector.only_generated_files selector
           , File_selector.predicate selector ))
    in
    let dep =
      let file = constr "file" path Dep.file in
      let env = constr "env" string (fun var -> Dep.env (Env.Var.of_string var)) in
      let alias =
        constr "alias" (pair build_path string) (fun (dir, name) ->
          Dep.alias (Alias.make (Alias.Name.of_string name) ~dir))
      in
      let glob = constr "glob" selector Dep.file_selector in
      sum
        [ econstr file; econstr env; econstr alias; econstr glob ]
        (function
          | Dep.File path -> case path file
          | Env var -> case (Env.Var.to_string var) env
          | Alias value ->
            case (Alias.dir value, Alias.Name.to_string (Alias.name value)) alias
          | File_selector selector -> case selector glob
          | Universe -> Code_error.raise "Cannot serialize a universe dependency" [])
    in
    let deps =
      constr
        "deps"
        (iso (list dep) Dep.Set.of_list Dep.Set.to_list)
        (fun deps -> `Deps deps)
    in
    let done_ = constr "done" unit (fun () -> `Done) in
    sum
      [ econstr deps; econstr done_ ]
      (function
        | `Deps values -> case values deps
        | `Done -> case () done_)
  ;;

  let initial rule_digest =
    Digest.Feed.compute_digest
      (Digest.Feed.tuple2 Digest.Feed.string Digest.Feed.digest)
      ("dynamic-dependency-manifest-v2", rule_digest)
  ;;

  let artifact_key key =
    Digest.Feed.compute_digest
      (Digest.Feed.tuple2 Digest.Feed.string Digest.Feed.digest)
      ("dynamic-artifacts-v1", key)
  ;;

  let advance key deps digest =
    let d = Digest.Manual.create () in
    Digest.Manual.string d "dynamic-dependency-step-v1";
    Digest.Manual.digest d key;
    Dep.Set.digest deps d;
    Digest.Manual.digest d digest;
    Digest.Manual.get d
  ;;

  let facts_digest facts ~env =
    let d = Digest.Manual.create () in
    Dep.Facts.digest facts d ~env;
    Digest.Manual.get d
  ;;

  (* Each node describes the next request. Its observed facts select the next
     node, so a changed observation never evaluates obsolete later requests.
     Missing nodes (including partially trimmed traces) are ordinary misses. *)
  let lookup ~rule_digest ~targets ~env ~build_deps =
    let rec loop key stages =
      match
        Dune_cache.Shared.Dynamic_deps.load ~rule_digest:key
        |> Option.bind ~f:(fun sexp ->
          Conv.of_sexp conv ~version:(0, 0) sexp |> Result.to_option)
      with
      | None ->
        Dune_trace.emit ~buffered:true Cache (fun () ->
          let reason =
            match !Dune_cache.Shared.config with
            | Disabled -> "cache disabled"
            | Enabled _ -> "dynamic dependency manifest unavailable"
          in
          Dune_trace.Event.Cache.shared
            (`Miss reason)
            ~rule_digest:(Digest.to_string key)
            ~head:(Targets.Validated.head targets));
        Fiber.return None
      | Some `Done ->
        Dune_cache.Shared.lookup
          ~can_go_in_shared_cache:true
          ~rule_digest:(artifact_key key)
          ~targets
        >>| Option.map ~f:(fun targets -> targets, List.rev stages)
      | Some (`Deps deps) ->
        let* digest = build_deps deps |> Memo.run >>| facts_digest ~env in
        loop (advance key deps digest) ((deps, digest) :: stages)
    in
    loop (initial rule_digest) []
  ;;

  let store ~rule_digest ~stages =
    let rec loop key = function
      | [] ->
        Dune_cache.Shared.Dynamic_deps.store ~rule_digest:key (Conv.to_sexp conv `Done);
        artifact_key key
      | (deps, digest) :: rest ->
        let manifest = Conv.to_sexp conv (`Deps deps) in
        Dune_cache.Shared.Dynamic_deps.store ~rule_digest:key manifest;
        loop (advance key deps digest) rest
    in
    match !Dune_cache.Shared.config with
    | Disabled -> rule_digest
    | Enabled _ -> loop (initial rule_digest) stages
  ;;
end

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
