open! Stdune
open Import
open Memo.Build.O
module Action_builder = Action_builder0

module Fs : sig
  (** A memoized version of [mkdir] that avoids calling [mkdir] multiple times
      in the same directory. *)
  val mkdir_p : Path.Build.t -> unit Memo.Build.t
end = struct
  let mkdir_p_memo =
    (* CR-someday amokhov: It's difficult to think about the correctness of this
       memoized function. Right now, we never invalidate it, so if we delete a
       stale build directory, we'll not be able to recreate it later. Perhaps,
       we should create directories as part of running actions that need them.
       That would be less efficient, as we'd call [mkdir] on the same directory
       multiple times, but it would be easier to guarantee correctness.

       Note: if we find a way to reliably invalidate this function, its output
       should continue to have no cutoff because the callers might depend not
       just on the existence of a directory but on its *continuous*
       existence. *)
    Memo.create "mkdir_p"
      ~input:(module Path.Build)
      (fun p ->
        Path.mkdir_p (Path.build p);
        Memo.Build.return ())

  let mkdir_p = Memo.exec mkdir_p_memo
end

module Loaded = struct
  type rules_here =
    { by_file_targets : Rule.t Path.Build.Map.t
    ; by_directory_targets : Rule.t Path.Build.Map.t
    }

  let no_rules_here =
    { by_file_targets = Path.Build.Map.empty
    ; by_directory_targets = Path.Build.Map.empty
    }

  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_produced : Rules.t
    ; rules_here : rules_here
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  type t =
    | Non_build of Path.Set.t
    | Build of build

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_produced = Rules.empty
      ; rules_here = no_rules_here
      ; aliases = Alias.Name.Map.empty
      }
end

module Subdir_set = struct
  type t =
    | All
    | These of String.Set.t

  let to_dir_set = function
    | All -> Dir_set.universal
    | These s ->
      String.Set.fold s ~init:Dir_set.empty ~f:(fun path acc ->
          let path = Path.Local.of_string path in
          Dir_set.union acc (Dir_set.singleton path))

  let of_dir_set d =
    match Dir_set.toplevel_subdirs d with
    | Infinite -> All
    | Finite s -> These s

  let of_list l = These (String.Set.of_list l)

  let empty = These String.Set.empty

  let mem t dir =
    match t with
    | All -> true
    | These t -> String.Set.mem t dir

  let union a b =
    match (a, b) with
    | All, _
    | _, All ->
      All
    | These a, These b -> These (String.Set.union a b)

  let inter_set = function
    | All -> Fun.id
    | These t -> String.Set.inter t

  let union_all = List.fold_left ~init:empty ~f:union
end

type extra_sub_directories_to_keep = Subdir_set.t

module Rule_fn = struct
  let loc_decl = Fdecl.create Dyn.opaque

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

module Dir_triage = struct
  type t =
    | Known of Loaded.t
    | Need_step2 of
        { dir : Path.Build.t
        ; context_or_install : Context_or_install.t
        ; sub_dir : Path.Source.t
        }
end

module Error = struct
  module Id = Id.Make ()

  type t =
    { exn : Exn_with_backtrace.t
    ; id : Id.t
    }

  let id t = t.id

  let promotion t =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg ->
      User_message.Annots.find msg.annots Diff_promotion.Annot.annot
    | _ -> None

  let info (t : t) =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg -> (
      let dir =
        User_message.Annots.find msg.annots Process.with_directory_annot
      in
      match User_message.Annots.find msg.annots Compound_user_error.annot with
      | None -> (msg, [], dir)
      | Some { main; related } -> (main, related, dir))
    | e ->
      (* CR-someday jeremiedimino: Use [Report_error.get_user_message] here. *)
      (User_message.make [ Pp.text (Printexc.to_string e) ], [], None)
end

module type Rule_generator = sig
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> (extra_sub_directories_to_keep * Rules.t) option Memo.Build.t

  val global_rules : Rules.t Memo.Lazy.t
end

module Handler = struct
  (* CR-someday amokhov: The name [Interrupt] is not precise, because the build
     is restarted, not, e.g. interrupted with Ctrl-C. Similarly, we have other
     imprecise names, like [Cancelled_due_to_file_changes] in [Scheduler] where
     the build is not just cancelled, it's restarted. We should make the naming
     more consistent. *)
  type event =
    | Start
    | Finish
    | Fail
    | Interrupt

  type error =
    | Add of Error.t
    | Remove of Error.t

  type t =
    { error : error list -> unit Fiber.t
    ; build_progress : complete:int -> remaining:int -> unit Fiber.t
    ; build_event : event -> unit Fiber.t
    }

  let report_progress t ~rule_done ~rule_total =
    t.build_progress ~complete:rule_done ~remaining:(rule_total - rule_done)

  let last_event : event option ref = ref None

  let report_build_event t evt =
    last_event := Some evt;
    t.build_event evt

  let do_nothing =
    { error = (fun _ -> Fiber.return ())
    ; build_progress = (fun ~complete:_ ~remaining:_ -> Fiber.return ())
    ; build_event = (fun _ -> Fiber.return ())
    }

  let create ~error ~build_progress ~build_event =
    { error; build_progress; build_event }
end

type t =
  { contexts : Build_context.t Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; mutable rule_done : int
  ; mutable rule_total : int
  ; mutable errors : Error.t list
  ; handler : Handler.t
  ; promote_source :
         chmod:(int -> int)
      -> delete_dst_if_it_is_a_directory:bool
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> Build_context.t option
      -> unit Fiber.t
  ; locks : (Path.t, Fiber.Mutex.t) Table.t
  ; build_mutex : Fiber.Mutex.t
  ; stats : Dune_stats.t option
  ; cache_config : Dune_cache.Config.t
  ; cache_debug_flags : Cache_debug_flags.t
  ; implicit_default_alias :
      Path.Build.t -> unit Action_builder.t option Memo.Build.t
  }

let t = Fdecl.create Dyn.opaque

let set x = Fdecl.set t x

let t () = Fdecl.get t

let errors () = (t ()).errors

let get_dir_triage t ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    let+ files = Source_tree.files_of dir in
    Dir_triage.Known (Non_build (Path.set_of_source_paths files))
  | External _ ->
    Memo.Build.return
    @@ Dir_triage.Known
         (Non_build
            (match Path.Untracked.readdir_unsorted dir with
            | Error (Unix.ENOENT, _, _) -> Path.Set.empty
            | Error (e, _syscall, _arg) ->
              (* CR-someday amokhov: Print [_syscall] and [_arg] too to help
                 debugging. *)
              User_warning.emit
                [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
                ; Pp.textf "Reason: %s" (Unix.error_message e)
                ];
              Path.Set.empty
            | Ok filenames -> Path.Set.of_listing ~dir ~filenames))
  | Build (Regular Root) ->
    let+ contexts = Memo.Lazy.force t.contexts in
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           (([ Dpath.Build.anonymous_actions_dir; Dpath.Build.install_dir ]
            |> List.map ~f:Path.Build.basename)
           @ (Context_name.Map.keys contexts
             |> List.map ~f:Context_name.to_string)))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let+ contexts = Memo.Lazy.force t.contexts in
    let allowed_subdirs =
      Context_name.Map.keys contexts
      |> List.map ~f:Context_name.to_string
      |> Subdir_set.of_list |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Anonymous_action p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Code_error.raise "Called get_dir_triage on an anonymous action directory"
      [ ("dir", Path.Build.to_dyn build_dir) ]
  | Build (Invalid _) ->
    Memo.Build.return
    @@ Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context (context_name, sub_dir))) ->
    (* In this branch, [dir] is in the build directory. *)
    let dir = Path.as_in_build_dir_exn dir in
    let context_or_install = Context_or_install.Install context_name in
    Memo.Build.return
      (Dir_triage.Need_step2 { dir; context_or_install; sub_dir })
  | Build (Regular (With_context (context_name, sub_dir))) ->
    (* In this branch, [dir] is in the build directory. *)
    let dir = Path.as_in_build_dir_exn dir in
    let context_or_install = Context_or_install.Context context_name in
    Memo.Build.return
      (Dir_triage.Need_step2 { dir; context_or_install; sub_dir })

let describe_rule (rule : Rule.t) =
  match rule.info with
  | From_dune_file { start; _ } ->
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy _ -> "file present in source tree"

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal
    | Source_file_copy _ ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise ~loc
    [ Pp.textf
        "This rule defines a target %S whose name conflicts with a source \
         directory in the same directory."
        (Path.Build.basename fn)
    ]
    ~hints:
      [ Pp.textf
          "If you want Dune to generate and replace %S, add (mode promote) to \
           the rule stanza. Alternatively, you can delete %S from the source \
           tree or change the rule to generate a different target."
          (Path.Build.basename fn) (Path.Build.basename fn)
      ]

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe_rule rule')
    ; Pp.textf "- %s" (describe_rule rule)
    ]
    ~hints:
      (match (rule.info, rule'.info) with
      | Source_file_copy _, _
      | _, Source_file_copy _ ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [])

(* All file targets of non-sandboxed actions that are currently being executed.
   On exit, we need to delete them as they might contain garbage. There is no
   [pending_dir_targets] since actions with directory targets are sandboxed. *)
let pending_file_targets = ref Path.Build.Set.empty

let () = Hooks.End_of_build.always Metrics.reset

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_file_targets in
      pending_file_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.Build.unlink_no_err p))

let rec with_locks t mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add t.locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks t mutexes ~f)

let remove_old_artifacts ~dir ~rules_here ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target =
          (* CR-someday amokhov: Also check directory targets. *)
          Path.Build.Map.mem rules_here.Loaded.by_file_targets path
        in
        if not path_is_a_target then
          match kind with
          | Unix.S_DIR -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
          | _ -> Path.unlink (Path.build path))

(* We don't remove files in there as we don't know upfront if they are stale or
   not. *)
let remove_old_sub_dirs_in_anonymous_actions_dir ~dir
    ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        match kind with
        | Unix.S_DIR -> (
          match subdirs_to_keep with
          | All -> ()
          | These set ->
            if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
        | _ -> ())

let no_rule_found t ~loc fn =
  let+ contexts = Memo.Lazy.force t.contexts in
  let fail fn ~loc =
    User_error.raise ?loc
      [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Anonymous_action _ ->
    (* We never lookup such actions by target name, so this should be
       unreachable *)
    Code_error.raise ?loc "Build_system.no_rule_found got anonymous action path"
      [ ("fn", Path.Build.to_dyn fn) ]

(* +-------------------- Adding rules to the system --------------------+ *)

let source_file_digest path =
  let report_user_error details =
    let+ loc = Rule_fn.loc () in
    User_error.raise ?loc
      ([ Pp.textf "File unavailable: %s" (Path.to_string_maybe_quoted path) ]
      @ details)
  in
  Fs_memo.path_digest path >>= function
  | Ok digest -> Memo.Build.return digest
  | No_such_file -> report_user_error []
  | Broken_symlink -> report_user_error [ Pp.text "Broken symlink" ]
  | Unexpected_kind st_kind ->
    report_user_error
      [ Pp.textf "This is neither a regular file nor a directory (%s)"
          (File_kind.to_string st_kind)
      ]
  | Unix_error (error, _, _) ->
    report_user_error [ Pp.textf "%s" (Unix.error_message error) ]
  | Error exn -> report_user_error [ Pp.textf "%s" (Printexc.to_string exn) ]

let eval_source_file :
    type a. a Action_builder.eval_mode -> Path.t -> a Memo.Build.t =
 fun mode path ->
  match mode with
  | Lazy -> Memo.Build.return ()
  | Eager ->
    let+ d = source_file_digest path in
    Dep.Fact.file path d

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.Build.t

  val file_exists : Path.t -> bool Memo.Build.t

  val file_targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

  val directory_targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

  val lookup_alias :
       Alias.t
    -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list option Memo.Build.t

  val alias_exists : Alias.t -> bool Memo.Build.t
end = struct
  open Load_rules

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list_map non_target_source_files ~f:(fun path ->
        let ctx_path = Path.Build.append_source ctx_dir path in
        let build =
          Action_builder.of_thunk
            { f =
                (fun mode ->
                  let path = Path.source path in
                  let+ fact = eval_source_file mode path in
                  ( Action.Full.make
                      (Action.copy path ctx_path)
                      (* There's an [assert false] in [prepare_managed_paths]
                         that blows up if we try to sandbox this. *)
                      ~sandbox:Sandbox_config.no_sandboxing
                  , Dep.Map.singleton (Dep.file path) fact ))
            }
        in
        Rule.make ~context:None ~info:(Source_file_copy path)
          ~targets:(Targets.File.create ctx_path)
          build)

  let compile_rules ~dir ~source_dirs rules =
    let file_targets, directory_targets =
      let check_for_source_dir_conflict rule target =
        if String.Set.mem source_dirs (Path.Build.basename target) then
          report_rule_src_dir_conflict dir target rule
      in
      List.map rules ~f:(fun rule ->
          assert (Path.Build.( = ) dir rule.Rule.dir);
          ( Path.Build.Set.to_list_map rule.targets.files ~f:(fun target ->
                check_for_source_dir_conflict rule target;
                (target, rule))
          , Path.Build.Set.to_list_map rule.targets.dirs ~f:(fun target ->
                check_for_source_dir_conflict rule target;
                (target, rule)) ))
      |> List.unzip
    in
    let by_file_targets =
      List.concat file_targets
      |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict
    in
    let by_directory_targets =
      List.concat directory_targets
      |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict
    in
    Path.Build.Map.iter2 by_file_targets by_directory_targets
      ~f:(fun target rule1 rule2 ->
        match (rule1, rule2) with
        | None, _
        | _, None ->
          ()
        | Some rule1, Some rule2 -> report_rule_conflict target rule1 rule2);
    { Loaded.by_file_targets; by_directory_targets }

  (* Here we are doing a O(log |S|) lookup in a set S of files in the build
     directory [dir]. We could memoize these lookups, but it doesn't seem to be
     worth it, since we're unlikely to perform exactly the same lookup many
     times. As far as I can tell, each lookup will be done twice: when computing
     static dependencies of a [Action_builder.t] with
     [Action_builder.static_deps] and when executing the very same
     [Action_builder.t] with [Action_builder.exec] -- the results of both
     [Action_builder.static_deps] and [Action_builder.exec] are cached. *)
  let file_exists fn =
    load_dir ~dir:(Path.parent_exn fn) >>| function
    | Non_build targets -> Path.Set.mem targets fn
    | Build { rules_here; _ } -> (
      match Path.as_in_build_dir fn with
      | None -> false
      | Some fn -> (
        match Path.Build.Map.mem rules_here.by_file_targets fn with
        | true -> true
        | false -> (
          match Path.Build.parent fn with
          | None -> false
          | Some dir -> Path.Build.Map.mem rules_here.by_directory_targets dir))
      )

  let file_targets_of ~dir =
    load_dir ~dir >>| function
    | Non_build file_targets -> file_targets
    | Build { rules_here; _ } ->
      Path.Build.Map.keys rules_here.by_file_targets
      |> Path.Set.of_list_map ~f:Path.build

  let directory_targets_of ~dir =
    load_dir ~dir >>| function
    | Non_build _file_targets -> Path.Set.empty
    | Build { rules_here; _ } ->
      Path.Build.Map.keys rules_here.by_directory_targets
      |> Path.Set.of_list_map ~f:Path.build

  let lookup_alias alias =
    load_dir ~dir:(Path.build (Alias.dir alias)) >>| function
    | Non_build _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)

  let alias_exists alias =
    lookup_alias alias >>| function
    | None -> false
    | Some _ -> true

  let compute_alias_expansions t ~(collected : Rules.Dir_rules.ready) ~dir =
    let aliases = collected.aliases in
    let+ aliases =
      if Alias.Name.Map.mem aliases Alias.Name.default then
        Memo.Build.return aliases
      else
        t.implicit_default_alias dir >>| function
        | None -> aliases
        | Some expansion ->
          Alias.Name.Map.set aliases Alias.Name.default
            { expansions =
                Appendable_list.singleton
                  (Loc.none, Rules.Dir_rules.Alias_spec.Deps expansion)
            }
    in
    Alias.Name.Map.map aliases
      ~f:(fun { Rules.Dir_rules.Alias_spec.expansions } ->
        Appendable_list.to_list expansions)

  let filter_out_fallback_rules ~to_copy rules =
    List.filter rules ~f:(fun (rule : Rule.t) ->
        match rule.mode with
        | Standard
        | Promote _
        | Ignore_source_files ->
          true
        | Fallback ->
          let source_files_for_targets =
            if not (Path.Build.Set.is_empty rule.targets.dirs) then
              Code_error.raise "Unexpected directory target in a Fallback rule"
                [ ("targets", Targets.Validated.to_dyn rule.targets) ];
            Path.Build.Set.to_list_map
              rule.targets.files
              (* All targets are in a directory of a build context since there
                 are source files to copy, so this call can't fail. *)
              ~f:Path.Build.drop_build_context_exn
            |> Path.Source.Set.of_list
          in
          if Path.Source.Set.is_subset source_files_for_targets ~of_:to_copy
          then
            (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targets to_copy)
          then
            (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targets to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targets absent_targets
            in
            User_error.raise ~loc:(Rule.loc rule)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; Pp.enumerate ~f:Path.pp
                  (Path.set_of_source_paths present_targets |> Path.Set.to_list)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; Pp.enumerate ~f:Path.pp
                  (Path.set_of_source_paths absent_targets |> Path.Set.to_list)
              ])

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction Memo.Build.t
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _
      | Alias _
      | Anonymous_action _
      | Other _ ->
        Memo.Build.return None
      | Regular (_ctx, sub_dir) -> Source_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      corresponding_source_dir ~dir >>| function
      | None -> String.Set.empty
      | Some dir -> Source_tree.Dir.sub_dir_names dir

    let allowed_dirs ~dir ~subdir : restriction Memo.Build.t =
      let+ subdirs = source_subdirs_of_build_dir ~dir in
      if String.Set.mem subdirs subdir then
        Unrestricted
      else
        Restricted
          (Memo.Lazy.create ~name:"allowed_dirs" (fun () ->
               load_dir ~dir:(Path.build dir) >>| function
               | Non_build _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  let load_dir_step2_exn t ~dir ~context_or_install ~sub_dir =
    let sub_dir_components = Path.Source.explode sub_dir in
    (* Load all the rules *)
    let (module RG : Rule_generator) = t.rule_generator in
    let* extra_subdirs_to_keep, rules_produced =
      RG.gen_rules context_or_install ~dir sub_dir_components >>| function
      | None ->
        Code_error.raise "[gen_rules] did not specify rules for the context"
          [ ("context_or_install", Context_or_install.to_dyn context_or_install)
          ]
      | Some x -> x
    and* global_rules = Memo.Lazy.force RG.global_rules in
    let rules =
      let dir = Path.build dir in
      Rules.Dir_rules.union
        (Rules.find rules_produced dir)
        (Rules.find global_rules dir)
    in
    let collected = Rules.Dir_rules.consume rules in
    let rules = collected.rules in
    (* Compute the set of sources and targets promoted to the source tree that
       must not be copied to the build directory. *)
    let source_files_to_ignore, source_dirnames_to_ignore =
      List.fold_left rules ~init:(Path.Build.Set.empty, String.Set.empty)
        ~f:(fun (acc_files, acc_dirnames) { Rule.targets; mode; loc; _ } ->
          let target_filenames =
            Path.Build.Set.to_list_map ~f:Path.Build.basename targets.files
            |> String.Set.of_list
          in
          let target_dirnames =
            Path.Build.Set.to_list_map ~f:Path.Build.basename targets.dirs
            |> String.Set.of_list
          in
          (* Check if this rule defines any directory targets that conflict with
             internal Dune directories listed in [extra_subdirs_to_keep]. *)
          (match
             String.Set.choose
               (Subdir_set.inter_set extra_subdirs_to_keep
                  (String.Set.union target_filenames target_dirnames))
           with
          | None -> ()
          | Some target_name ->
            User_error.raise ~loc
              [ Pp.textf
                  "This rule defines a target %S whose name conflicts with an \
                   internal directory used by Dune. Please use a different \
                   name."
                  target_name
              ]);
          match mode with
          | Ignore_source_files ->
            ( Path.Build.Set.union acc_files targets.files
            , String.Set.union acc_dirnames target_dirnames )
          | Promote { only; _ } ->
            (* Note that the [only] predicate applies to the files inside the
               directory targets rather than to directory names themselves. *)
            let target_files =
              match only with
              | None -> targets.files
              | Some pred ->
                let is_promoted file =
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build file) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any
                in
                Path.Build.Set.filter targets.files ~f:is_promoted
            in
            ( Path.Build.Set.union acc_files target_files
            , String.Set.union acc_dirnames target_dirnames )
          | Standard
          | Fallback ->
            (acc_files, acc_dirnames))
    in
    (* Take into account the source files *)
    let* to_copy, source_dirs =
      match context_or_install with
      | Install _ -> Memo.Build.return (None, String.Set.empty)
      | Context context_name ->
        let+ files, subdirs =
          Source_tree.find_dir sub_dir >>| function
          | None -> (Path.Source.Set.empty, String.Set.empty)
          | Some dir ->
            (Source_tree.Dir.file_paths dir, Source_tree.Dir.sub_dir_names dir)
        in
        let files =
          let source_files_to_ignore =
            Path.Build.Set.to_list_map ~f:Path.Build.drop_build_context_exn
              source_files_to_ignore
            |> Path.Source.Set.of_list
          in
          let source_files_to_ignore =
            Target_promotion.delete_stale_dot_merlin_file ~dir
              ~source_files_to_ignore
          in
          Path.Source.Set.diff files source_files_to_ignore
        in
        let subdirs = String.Set.diff subdirs source_dirnames_to_ignore in
        if Path.Source.Set.is_empty files then
          (None, subdirs)
        else
          let ctx_path = Context_name.build_dir context_name in
          (Some (ctx_path, files), subdirs)
    in
    (* Filter out fallback rules *)
    let rules =
      match to_copy with
      | None ->
        (* If there are no source files to copy, fallback rules are
           automatically kept *)
        rules
      | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy rules
    in
    (* Compile the rules and cleanup stale artifacts *)
    let rules =
      (match to_copy with
      | None -> []
      | Some (ctx_dir, source_files) ->
        create_copy_rules ~ctx_dir ~non_target_source_files:source_files)
      @ rules
    in
    let* allowed_by_parent =
      match (context_or_install, sub_dir_components) with
      | Context _, [ ".dune" ] ->
        (* GROSS HACK: this is to avoid a cycle as the rules for all directories
           force the generation of ".dune/configurator". We need a better way to
           deal with such cases. *)
        Memo.Build.return Generated_directory_restrictions.Unrestricted
      | _ -> Generated_directory_restrictions.allowed_by_parent ~dir
    in
    let* () =
      match allowed_by_parent with
      | Unrestricted -> Memo.Build.return ()
      | Restricted restriction -> (
        match Path.Build.Map.find (Rules.to_map rules_produced) dir with
        | None -> Memo.Build.return ()
        | Some rules ->
          let+ restriction = Memo.Lazy.force restriction in
          if not (Dir_set.here restriction) then
            Code_error.raise
              "Generated rules in a directory not allowed by the parent"
              [ ("dir", Path.Build.to_dyn dir)
              ; ("rules", Rules.Dir_rules.to_dyn rules)
              ])
    in
    let* descendants_to_keep =
      let rules_generated_in =
        Rules.to_map rules_produced
        |> Path.Build.Map.foldi ~init:Dir_set.empty ~f:(fun p _ acc ->
               match Path.Local_gen.descendant ~of_:dir p with
               | None -> acc
               | Some p -> Dir_set.union acc (Dir_set.singleton p))
      in
      let subdirs_to_keep =
        match extra_subdirs_to_keep with
        | All -> Subdir_set.All
        | These set -> These (String.Set.union source_dirs set)
      in
      let+ allowed_grand_descendants_of_parent =
        match allowed_by_parent with
        | Unrestricted ->
          (* In this case the parent isn't going to be able to create any
             generated grand descendant directories. Rules that attempt to do so
             may run into the [allowed_by_parent] check or will be simply
             ignored. *)
          Memo.Build.return Dir_set.empty
        | Restricted restriction -> Memo.Lazy.force restriction
      in
      Dir_set.union_all
        [ rules_generated_in
        ; Subdir_set.to_dir_set subdirs_to_keep
        ; allowed_grand_descendants_of_parent
        ]
    in
    let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
    let rules_here = compile_rules ~dir ~source_dirs rules in
    remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
    remove_old_sub_dirs_in_anonymous_actions_dir
      ~dir:
        (Path.Build.append_local Dpath.Build.anonymous_actions_dir
           (Path.Build.local dir))
      ~subdirs_to_keep;
    let+ aliases =
      match context_or_install with
      | Context _ -> compute_alias_expansions t ~collected ~dir
      | Install _ ->
        (* There are no aliases in the [_build/install] directory *)
        Memo.Build.return Alias.Name.Map.empty
    in
    { Loaded.allowed_subdirs = descendants_to_keep
    ; rules_produced
    ; rules_here
    ; aliases
    }

  let load_dir_impl t ~dir : Loaded.t Memo.Build.t =
    get_dir_triage t ~dir >>= function
    | Known l -> Memo.Build.return l
    | Need_step2 { dir; context_or_install; sub_dir } ->
      let+ build = load_dir_step2_exn t ~dir ~context_or_install ~sub_dir in
      Loaded.Build build

  let load_dir =
    let load_dir_impl dir = load_dir_impl (t ()) ~dir in
    let memo = Memo.create "load-dir" ~input:(module Path) load_dir_impl in
    fun ~dir -> Memo.exec memo dir
end

open Load_rules

let load_dir_and_get_buildable_targets ~dir =
  load_dir ~dir >>| function
  | Non_build _ -> Loaded.no_rules_here
  | Build { rules_here; _ } -> rules_here

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_rule_for_directory_target path =
  let rec loop dir =
    match Path.Build.parent dir with
    | None -> Memo.Build.return None
    | Some parent_dir -> (
      let* rules =
        load_dir_and_get_buildable_targets ~dir:(Path.build parent_dir)
      in
      match Path.Build.Map.find rules.by_directory_targets dir with
      | None -> loop parent_dir
      | Some _ as rule -> Memo.Build.return rule)
  in
  loop path

let get_rule path =
  match Path.as_in_build_dir path with
  | None -> Memo.Build.return None
  | Some path -> (
    let dir = Path.Build.parent_exn path in
    load_dir ~dir:(Path.build dir) >>= function
    | Non_build _ -> assert false
    | Build { rules_here; _ } -> (
      match Path.Build.Map.find rules_here.by_file_targets path with
      | Some _ as rule -> Memo.Build.return rule
      | None -> get_rule_for_directory_target path))

let get_rule_or_source t path =
  let dir = Path.parent_exn path in
  if Path.is_strict_descendant_of_build_dir dir then
    let* rules = load_dir_and_get_buildable_targets ~dir in
    let path = Path.as_in_build_dir_exn path in
    match Path.Build.Map.find rules.by_file_targets path with
    | Some rule -> Memo.Build.return (Rule (path, rule))
    | None -> (
      get_rule_for_directory_target path >>= function
      | Some rule -> Memo.Build.return (Rule (path, rule))
      | None ->
        let* loc = Rule_fn.loc () in
        no_rule_found t ~loc path)
  else
    let+ d = source_file_digest path in
    Source d

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Memo.Build) (Monoid.Union (Path.Build.Set))

let all_targets t =
  let* root = Source_tree.root ()
  and* contexts = Memo.Lazy.force t.contexts in
  Memo.Build.parallel_map (Context_name.Map.values contexts) ~f:(fun ctx ->
      Source_tree_map_reduce.map_reduce root ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir ->
          load_dir
            ~dir:
              (Path.build
                 (Path.Build.append_source ctx.Build_context.build_dir
                    (Source_tree.Dir.path dir)))
          >>| function
          | Non_build _ -> Path.Build.Set.empty
          | Build { rules_here; _ } ->
            Path.Build.Set.of_list
              (Path.Build.Map.keys rules_here.by_file_targets
              @ Path.Build.Map.keys rules_here.by_directory_targets)))
  >>| Path.Build.Set.union_all

let get_alias_definition alias =
  lookup_alias alias >>= function
  | None ->
    let open Pp.O in
    let+ loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.text "No rule found for " ++ Alias.describe alias ]
  | Some x -> Memo.Build.return x

type rule_execution_result =
  { deps : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Path.Build.Map.t
  }

module type Rec = sig
  (** Build all the transitive dependencies of the alias and return the alias
      expansion. *)
  val build_alias : Alias.t -> Dep.Fact.Files.t Memo.Build.t

  val build_file : Path.t -> Digest.t Memo.Build.t

  val build_dir :
    Path.t -> (Digest.t * Digest.t Path.Build.Map.t) option Memo.Build.t

  val build_deps : Dep.Set.t -> Dep.Facts.t Memo.Build.t

  val eval_deps :
    'a Action_builder.eval_mode -> Dep.Set.t -> 'a Dep.Map.t Memo.Build.t

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  val execute_action :
    observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.Build.t

  val execute_action_stdout :
       observing_facts:Dep.Facts.t
    -> Rule.Anonymous_action.t
    -> string Memo.Build.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t Memo.Build.t

    val build : File_selector.t -> Dep.Fact.Files.t Memo.Build.t
  end
end

let is_target file =
  match Path.is_in_build_dir file with
  | false -> Memo.Build.return false
  | true -> (
    let parent_dir = Path.parent_exn file in
    let* file_targets = file_targets_of ~dir:parent_dir in
    match Path.Set.mem file_targets file with
    | true -> Memo.Build.return true
    | false ->
      let rec loop dir =
        match Path.parent dir with
        | None -> Memo.Build.return false
        | Some parent_dir -> (
          let* directory_targets = directory_targets_of ~dir:parent_dir in
          match Path.Set.mem directory_targets dir with
          | true -> Memo.Build.return true
          | false -> loop parent_dir)
      in
      loop file)

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (i.e. only expose
   functions). *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  (* The below two definitions are useless, but if we remove them we get an
     "Undefined_recursive_module" exception. *)

  val build_file_memo :
    (Path.t, Import.Digest.t * Import.Digest.t Path.Build.Map.t option) Memo.t
    [@@warning "-32"]

  val build_alias_memo : (Alias.t, Dep.Fact.Files.t) Memo.t [@@warning "-32"]

  val dep_on_alias_definition :
    Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t
end = struct
  open Used_recursively

  (* [build_dep] turns a [Dep.t] which is a description of a dependency into a
     fact about the world. To do that, it needs to do some building. *)
  let build_dep : Dep.t -> Dep.Fact.t Memo.Build.t = function
    | Alias a ->
      let+ digests = build_alias a in
      (* Fact: alias [a] expands to the set of file-digest pairs [digests] *)
      Dep.Fact.alias a digests
    | File f ->
      let+ digest = build_file f in
      (* Fact: file [f] has digest [digest] *)
      Dep.Fact.file f digest
    | File_selector g ->
      let+ digests = Pred.build g in
      (* Fact: file selector [g] expands to the set of file- and (possibly)
         dir-digest pairs [digests] *)
      Dep.Fact.file_selector g digests
    | Universe
    | Env _ ->
      (* Facts about these dependencies are constructed in
         [Dep.Facts.digest]. *)
      Memo.Build.return Dep.Fact.nothing

  let build_deps deps =
    Dep.Map.parallel_map deps ~f:(fun dep () -> build_dep dep)

  let eval_deps :
      type a.
      a Action_builder.eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.Build.t =
   fun mode deps ->
    match mode with
    | Lazy -> Memo.Build.return deps
    | Eager -> build_deps deps

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    let evaluate_sandboxing_preference preference =
      let use_copy_on_windows mode =
        match Sandbox_mode.Set.mem config Sandbox_mode.copy with
        | true ->
          Some
            (if Sys.win32 then
              Sandbox_mode.copy
            else
              mode)
        | false ->
          User_error.raise ~loc
            [ Pp.textf
                "This rule requires sandboxing with %ss, but that won't work \
                 on Windows."
                (Sandbox_mode.to_string mode)
            ]
      in
      match Sandbox_mode.Set.mem config preference with
      | false -> None
      | true -> (
        match preference with
        | Some Symlink -> use_copy_on_windows Sandbox_mode.symlink
        | Some Hardlink -> use_copy_on_windows Sandbox_mode.hardlink
        | _ -> Some preference)
    in
    match
      List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
    with
    | Some choice -> choice
    | None ->
      (* This is not trivial to reach because the user rules are checked at
         parse time and [sandboxing_preference] always includes all possible
         modes. However, it can still be reached if multiple sandbox config
         specs are combined into an unsatisfiable one. *)
      User_error.raise ~loc
        [ Pp.text
            "This rule forbids all sandboxing modes (but it also requires \
             sandboxing)"
        ]

  let start_rule t _rule = t.rule_total <- t.rule_total + 1

  (* The current version of the rule digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let rule_digest_version = 10

  let compute_rule_digest (rule : Rule.t) ~deps ~action ~sandbox_mode
      ~execution_parameters =
    let { Action.Full.action
        ; env
        ; locks
        ; can_go_in_shared_cache
        ; sandbox = _ (* already taken into account in [sandbox_mode] *)
        } =
      action
    in
    let file_targets =
      Path.Build.Set.to_list_map rule.targets.files ~f:Path.Build.to_string
    in
    let dir_targets =
      Path.Build.Set.to_list_map rule.targets.dirs ~f:Path.Build.to_string
    in
    let trace =
      ( rule_digest_version (* Update when changing the rule digest scheme. *)
      , sandbox_mode
      , Dep.Facts.digest deps ~env
      , file_targets @ dir_targets
      , Option.map rule.context ~f:(fun c -> Context_name.to_string c.name)
      , Action.for_shell action
      , can_go_in_shared_cache
      , List.map locks ~f:Path.to_string
      , Execution_parameters.action_stdout_on_success execution_parameters
      , Execution_parameters.action_stderr_on_success execution_parameters )
    in
    Digest.generic trace

  let report_evaluated_rule build_system =
    Option.iter build_system.stats ~f:(fun stats ->
        let module Event = Chrome_trace.Event in
        let event =
          let args = [ ("value", `Int build_system.rule_total) ] in
          let ts = Event.Timestamp.now () in
          let common = Event.common_fields ~name:"evaluated_rules" ~ts () in
          Event.counter common args
        in
        Dune_stats.emit stats event)

  module Exec_result = struct
    type t =
      { produced_targets : unit Targets.Produced.t
      ; action_exec_result : Action_exec.Exec_result.t
      }
  end

  type rule_kind =
    | Normal_rule
    | Anonymous_action of
        { stamp_file : Path.Build.t
        ; capture_stdout : bool
        ; attached_to_alias : bool
        }

  let execute_action_for_rule t ~rule_kind ~rule_digest ~action ~deps ~loc
      ~(context : Build_context.t option) ~execution_parameters ~sandbox_mode
      ~dir ~(targets : Targets.Validated.t) : Exec_result.t Fiber.t =
    let open Fiber.O in
    let { Action.Full.action
        ; env
        ; locks
        ; can_go_in_shared_cache = _
        ; sandbox = _
        } =
      action
    in
    let sandbox =
      match sandbox_mode with
      | Some mode ->
        Some
          (Sandbox.create ~mode ~deps ~rule_dir:dir ~rule_loc:loc ~rule_digest
             ~expand_aliases:
               (Execution_parameters.expand_aliases_in_sandbox
                  execution_parameters))
      | None ->
        (* If the action is not sandboxed, we use [pending_file_targets] to
           clean up the build directory if the action is interrupted. *)
        pending_file_targets :=
          Path.Build.Set.union targets.files !pending_file_targets;
        None
    in
    let action =
      match sandbox with
      | None ->
        (* CR-someday amokhov: It may be possible to support directory targets
           without sandboxing. We just need to make sure we clean up all stale
           directory targets before running the rule and then we can discover
           all created files right in the build directory. *)
        if not (Path.Build.Set.is_empty targets.dirs) then
          User_error.raise ~loc
            [ Pp.text "Rules with directory targets must be sandboxed." ];
        action
      | Some sandbox -> Action.sandbox action sandbox
    in
    let action =
      (* We must add the creation of the stamp file after sandboxing it, as
         otherwise the stamp file would end up inside the sandbox. This is
         especially a problem for the [Patch_back_source_tree] sandboxing
         mode. *)
      match rule_kind with
      | Normal_rule -> action
      | Anonymous_action { stamp_file; capture_stdout; _ } ->
        if capture_stdout then
          Action.with_stdout_to stamp_file action
        else
          Action.progn [ action; Action.write_file stamp_file "" ]
    in
    let* () =
      let chdirs = Action.chdirs action in
      Fiber.sequential_iter_seq (Path.Set.to_seq chdirs) ~f:(fun path ->
          match Path.as_in_build_dir path with
          | Some path -> Memo.Build.run (Fs.mkdir_p path)
          | None ->
            Code_error.raise ~loc
              (sprintf
                 "Can't [chdir] to %S because it's outside the build directory"
                 (Path.to_string path))
              [])
    in
    let root =
      match context with
      | None -> Path.Build.root
      | Some context -> context.build_dir
    in
    let root =
      Path.build
        (match sandbox with
        | None -> root
        | Some sandbox -> Sandbox.map_path sandbox root)
    in
    let+ exec_result =
      with_locks t locks ~f:(fun () ->
          let build_deps deps = Memo.Build.run (build_deps deps) in
          let+ action_exec_result =
            Action_exec.exec ~root ~context ~env ~targets:(Some targets)
              ~rule_loc:loc ~build_deps ~execution_parameters action
          in
          let produced_targets =
            match sandbox with
            | None ->
              (* Directory targets are not allowed for non-sandboxed actions, so
                 the call below should not raise. *)
              Targets.Produced.of_validated_files targets ~on_dir_target:`Raise
            | Some sandbox ->
              (* The stamp file for anonymous actions is always created outside
                 the sandbox, so we can't move it. *)
              let should_be_skipped =
                match rule_kind with
                | Normal_rule -> fun (_ : Path.Build.t) -> false
                | Anonymous_action { stamp_file; _ } ->
                  Path.Build.equal stamp_file
              in
              Sandbox.move_targets_to_build_dir sandbox ~loc ~should_be_skipped
                ~targets
          in
          { Exec_result.produced_targets; action_exec_result })
    in
    (match sandbox with
    | Some sandbox -> Sandbox.destroy sandbox
    | None ->
      (* All went well, these targets are no longer pending. *)
      pending_file_targets :=
        Path.Build.Set.diff !pending_file_targets targets.files);
    exec_result

  let promote_targets t ~rule_mode ~dir ~targets ~context =
    match (rule_mode, !Clflags.promote) with
    | (Rule.Mode.Standard | Fallback | Ignore_source_files), _
    | Promote _, Some Never ->
      Fiber.return ()
    | Promote promote, (Some Automatically | None) ->
      Target_promotion.promote ~dir ~targets ~promote
        ~promote_source:(t.promote_source context)

  let execute_rule_impl ~rule_kind rule =
    let t = t () in
    let { Rule.id = _; targets; dir; context; mode; action; info = _; loc } =
      rule
    in
    start_rule t rule;
    let head_target = Targets.Validated.head targets in
    let* execution_parameters =
      match Dpath.Target_dir.of_target dir with
      | Regular (With_context (_, dir))
      | Anonymous_action (With_context (_, dir)) ->
        Source_tree.execution_parameters_of_dir dir
      | _ -> Execution_parameters.default
    in
    (* Note: we do not run the below in parallel with the above: if we fail to
       compute action execution parameters, we have no use for the action and
       might as well fail early, skipping unnecessary dependencies. The function
       [Source_tree.execution_parameters_of_dir] is memoized, and the result is
       not expected to change often, so we do not sacrifice too much performance
       here by executing it sequentially. *)
    let* action, deps = Action_builder.run action Eager in
    let wrap_fiber f =
      Memo.Build.of_reproducible_fiber
        (if Loc.is_none loc then
          f ()
        else
          Fiber.with_error_handler f ~on_error:(fun exn ->
              match exn.exn with
              | User_error.E msg when not (User_message.has_location msg) ->
                let msg = { msg with loc = Some loc } in
                Exn_with_backtrace.reraise { exn with exn = User_error.E msg }
              | _ -> Exn_with_backtrace.reraise exn))
    in
    wrap_fiber (fun () ->
        let open Fiber.O in
        report_evaluated_rule t;
        let* () = Memo.Build.run (Fs.mkdir_p dir) in
        let is_action_dynamic = Action.is_dynamic action.action in
        let sandbox_mode =
          match Action.is_useful_to_sandbox action.action with
          | Clearly_not ->
            if Sandbox_config.mem action.sandbox Sandbox_mode.none then
              Sandbox_mode.none
            else
              User_error.raise ~loc
                [ Pp.text
                    "Rule dependencies are configured to require sandboxing, \
                     but the rule has no actions that could potentially \
                     require sandboxing."
                ]
          | Maybe ->
            select_sandbox_mode ~loc action.sandbox
              ~sandboxing_preference:t.sandboxing_preference
        in
        (* CR-someday amokhov: More [always_rerun] and [can_go_in_shared_cache]
           to [Rule_cache] too. *)
        let always_rerun =
          let is_test =
            (* jeremiedimino: what about:

               {v (rule (alias runtest) (targets x) (action ...)) v}

               These will be treated as [Normal_rule], and the below match means
               that [--force] will have no effect on them. Is that what we want?

               The doc says:

               -f, --force Force actions associated to aliases to be re-executed
               even if their dependencies haven't changed.

               So it seems to me that such rules should be re-executed. TBC *)
            match rule_kind with
            | Normal_rule -> false
            | Anonymous_action a -> a.attached_to_alias
          in
          let force_rerun = !Clflags.force && is_test in
          force_rerun || Dep.Map.has_universe deps
        in
        let rule_digest =
          compute_rule_digest rule ~deps ~action ~sandbox_mode
            ~execution_parameters
        in
        (* CR-someday amokhov: Add support for rules with directory targets. *)
        let can_go_in_shared_cache =
          action.can_go_in_shared_cache
          && Path.Build.Set.is_empty targets.dirs
          && (not
                (always_rerun || is_action_dynamic
                || Action.is_useful_to_memoize action.action = Clearly_not))
          &&
          match sandbox_mode with
          | Some Patch_back_source_tree ->
            (* Action in this mode cannot go in the shared cache *)
            false
          | _ -> true
        in
        let* (produced_targets : Digest.t Targets.Produced.t) =
          (* Step I. Check if the workspace-local cache is up to date. *)
          Rule_cache.Workspace_local.lookup ~always_rerun ~rule_digest
            ~print_debug_info:t.cache_debug_flags.workspace_local_cache ~targets
            ~env:action.env ~build_deps
          >>= function
          | Some produced_targets -> Fiber.return produced_targets
          | None ->
            (* Step II. Remove stale targets both from the digest table and from
               the build directory. *)
            Path.Build.Set.iter targets.files ~f:(fun file ->
                Cached_digest.remove file;
                Path.Build.unlink_no_err file);
            Path.Build.Set.iter targets.dirs ~f:(fun dir ->
                Cached_digest.remove dir;
                Path.rm_rf (Path.build dir));
            let* produced_targets, dynamic_deps_stages =
              (* Step III. Try to restore artifacts from the shared cache. *)
              match
                Rule_cache.Shared.lookup ~can_go_in_shared_cache
                  ~cache_config:t.cache_config
                  ~print_debug_info:t.cache_debug_flags.shared_cache
                  ~rule_digest ~targets ~target_dir:rule.dir
              with
              | Some produced_targets ->
                (* Rules with dynamic deps can't be stored to the shared cache
                   (see the [is_action_dynamic] check above), so we know this is
                   not a dynamic action, so returning an empty list is correct.
                   The lack of information to fill in [dynamic_deps_stages] here
                   is precisely the reason why we don't store dynamic actions in
                   the shared cache. *)
                let dynamic_deps_stages = [] in
                Fiber.return (produced_targets, dynamic_deps_stages)
              | None ->
                (* Step IV. Execute the build action. *)
                let* exec_result =
                  execute_action_for_rule t ~rule_kind ~rule_digest ~action
                    ~deps ~loc ~context ~execution_parameters ~sandbox_mode ~dir
                    ~targets
                in
                (* Step V. Examine produced targets and store them to the shared
                   cache if needed. *)
                let* produced_targets =
                  Rule_cache.Shared.examine_targets_and_store
                    ~cache_config:t.cache_config ~can_go_in_shared_cache ~loc
                    ~rule_digest ~execution_parameters
                    ~produced_targets:exec_result.produced_targets
                    ~action:action.action
                in
                let dynamic_deps_stages =
                  List.map exec_result.action_exec_result.dynamic_deps_stages
                    ~f:(fun (deps, fact_map) ->
                      (deps, Dep.Facts.digest fact_map ~env:action.env))
                in
                Fiber.return (produced_targets, dynamic_deps_stages)
            in
            (* We do not include target names into [targets_digest] because they
               are already included into the rule digest. *)
            Rule_cache.Workspace_local.store ~head_target ~rule_digest
              ~dynamic_deps_stages
              ~targets_digest:(Targets.Produced.digest produced_targets);
            Fiber.return produced_targets
        in
        let* () =
          promote_targets t ~rule_mode:mode ~dir ~targets:produced_targets
            ~context
        in
        t.rule_done <- t.rule_done + 1;
        let+ () =
          Handler.report_progress t.handler ~rule_done:t.rule_done
            ~rule_total:t.rule_total
        in
        produced_targets)
    (* jeremidimino: We need to include the dependencies discovered while
       running the action here. Otherwise, package dependencies are broken in
       the presence of dynamic actions. *)
    >>|
    fun produced_targets ->
    { deps; targets = Targets.Produced.all_files produced_targets }

  module Anonymous_action = struct
    type t =
      { action : Rule.Anonymous_action.t
      ; deps : Dep.Set.t
      ; capture_stdout : bool
      ; digest : Digest.t
      }

    let equal a b = Digest.equal a.digest b.digest

    let hash t = Digest.hash t.digest

    let to_dyn t : Dyn.t =
      Record
        [ ("digest", Digest.to_dyn t.digest)
        ; ("loc", Dyn.option Loc.to_dyn t.action.loc)
        ]
  end

  let execute_action_generic_stage2_impl
      { Anonymous_action.action = act; deps; capture_stdout; digest } =
    let target =
      let dir =
        Path.Build.append_local Dpath.Build.anonymous_actions_dir
          (Path.Build.local act.dir)
      in
      let d = Digest.to_string digest in
      let basename =
        match act.alias with
        | None -> d
        | Some a -> Alias.Name.to_string a ^ "-" ^ d
      in
      Path.Build.relative dir basename
    in
    let rule =
      let { Rule.Anonymous_action.context; action = _; loc; dir = _; alias = _ }
          =
        act
      in
      Rule.make ~context
        ~info:
          (match loc with
          | Some loc -> From_dune_file loc
          | None -> Internal)
        ~targets:(Targets.File.create target)
        ~mode:Standard
        (Action_builder.of_thunk
           { f =
               (fun mode ->
                 let+ deps = eval_deps mode deps in
                 (act.action, deps))
           })
    in
    let+ { deps = _; targets = _ } =
      execute_rule_impl rule
        ~rule_kind:
          (Anonymous_action
             { attached_to_alias = Option.is_some act.alias
             ; capture_stdout
             ; stamp_file = target
             })
    in
    target

  let execute_action_generic_stage2_memo =
    Memo.create "execute-action"
      ~input:(module Anonymous_action)
      execute_action_generic_stage2_impl

  let execute_action_generic ~observing_facts (act : Rule.Anonymous_action.t)
      ~capture_stdout =
    (* We memoize the execution of anonymous actions, both via the persistent
       mechanism for not re-running build rules between invocations of [dune
       build] and via [Memo]. The former is done by producing a normal build
       rule on the fly for the anonymous action.

       Memoizing such actions via [Memo] doesn't feel super useful given that we
       expect a given anonymous action to be executed only once in a given
       build. And so persistent mechanism should be enough.

       However, if it does happen that two code paths try to execute the same
       anonymous action, then we need to be sure it is not executed twice. This
       is because the build rule we produce on the fly creates a file whose name
       only depend on the action. If the two execution could run concurrently,
       then they would both try to create the same file. So in this regard, we
       use [Memo] mostly for synchronisation purposes. *)
    (* Here we "forget" the facts about the world. We do that to make the input
       of the memoized function smaller. If we passed the whole [original_facts]
       as input, then we would end up memoizing one entry per set of facts. This
       could use a lot of memory. For instance, if we used [action_stdout] for
       the calls to [ocamldep], then Dune would remember the whole history of
       calls to [ocamldep] for each OCaml source file. *)
    let deps = Dep.Map.map observing_facts ~f:ignore in
    (* Shadow [observing_facts] to make sure we don't use it again. *)
    let observing_facts = () in
    ignore observing_facts;
    let digest =
      let { Rule.Anonymous_action.context
          ; action = { action; env; locks; can_go_in_shared_cache; sandbox }
          ; loc
          ; dir
          ; alias
          } =
        act
      in
      let env =
        (* Here we restrict the environment to only the variables we depend on,
           so that we don't re-execute all actions when some irrelevant
           environment variable changes.

           Ideally, we would pass this restricted environment to the external
           command, however that might be tedious to do in practice. See this
           ticket for a longer discussion about the management of the
           environment: https://github.com/ocaml/dune/issues/4382 *)
        Dep.Set.fold deps ~init:Env.Map.empty ~f:(fun dep acc ->
            match dep with
            | Env var -> Env.Map.set acc var (Env.get env var)
            | _ -> acc)
        |> Env.Map.to_list
      in
      Digest.generic
        ( Option.map context ~f:(fun c ->
              (* Only looking at the context name is fishy, but it is in line
                 with what we do for build rules. *)
              Context_name.to_string c.name)
        , env
        , Dep.Set.digest deps
        , Action.for_shell action
        , List.map locks ~f:Path.to_string
        , loc
        , dir
        , alias
        , capture_stdout
        , can_go_in_shared_cache
        , sandbox )
    in
    (* It might seem superfluous to memoize the execution here, given that a
       given anonymous action will typically only appear once during a given
       build. However, it is possible that two code paths try to execute the
       exact same anonymous action, and so would end up trying to create the
       same file. Using [Memo.create] serves as a synchronisation point to share
       the execution and avoid such a race condition. *)
    Memo.exec execute_action_generic_stage2_memo
      { action = act; deps; capture_stdout; digest }

  let execute_action ~observing_facts act =
    let+ _target =
      execute_action_generic ~observing_facts act ~capture_stdout:false
    in
    ()

  let execute_action_stdout ~observing_facts act =
    let+ target =
      execute_action_generic ~observing_facts act ~capture_stdout:true
    in
    Io.read_file (Path.build target)

  (* A rule can have multiple targets but calls to [execute_rule] are memoized,
     so the rule will be executed only once.

     [build_file_impl] returns both the set of dependencies of the file as well
     as its digest. *)
  let build_file_impl path =
    let t = t () in
    get_rule_or_source t path >>= function
    | Source digest -> Memo.Build.return (digest, None)
    | Rule (path, rule) -> (
      let+ { deps = _; targets } =
        Memo.push_stack_frame
          (fun () -> execute_rule rule)
          ~human_readable_description:(fun () ->
            Pp.text (Path.to_string_maybe_quoted (Path.build path)))
      in
      match Path.Build.Map.find targets path with
      | Some digest -> (digest, None)
      | None -> (
        match Cached_digest.build_file path with
        | Ok digest -> (digest, Some targets) (* Must be a directory target *)
        | No_such_file
        | Broken_symlink
        | Unexpected_kind _
        | Unix_error _
        | Error _ ->
          (* CR-someday amokhov: The most important reason we end up here is
             [No_such_file]. I think some of the outcomes above are impossible
             but some others will benefit from a better error. To be refined. *)
          let target =
            Path.Build.drop_build_context_exn path
            |> Path.Source.to_string_maybe_quoted
          in
          let matching_dirs =
            Path.Build.Set.to_list_map rule.targets.dirs ~f:(fun dir ->
                match Path.Build.is_descendant path ~of_:dir with
                | true -> [ dir ]
                | false -> [])
            |> List.concat
          in
          let matching_target =
            match matching_dirs with
            | [ dir ] ->
              Path.Build.drop_build_context_exn dir
              |> Path.Source.to_string_maybe_quoted
            | []
            | _ :: _ ->
              Code_error.raise "Multiple matching directory targets"
                [ ("targets", Targets.Validated.to_dyn rule.targets) ]
          in
          User_error.raise ~loc:rule.loc
            ~annots:
              (User_message.Annots.singleton
                 User_message.Annots.needs_stack_trace ())
            [ Pp.textf
                "This rule defines a directory target %S that matches the \
                 requested path %S but the rule's action didn't produce it"
                matching_target target
            ]))

  let dep_on_anonymous_action (x : Rule.Anonymous_action.t Action_builder.t) :
      _ Action_builder.t =
    Action_builder.of_thunk
      { f =
          (fun (type m) (mode : m Action_builder.eval_mode) ->
            match mode with
            | Lazy -> Memo.Build.return ((), Dep.Map.empty)
            | Eager ->
              let* action, facts = Action_builder.run x Eager in
              let+ () = execute_action action ~observing_facts:facts in
              ((), Dep.Map.empty))
      }

  let dep_on_alias_definition (definition : Rules.Dir_rules.Alias_spec.item) =
    match definition with
    | Deps x -> x
    | Action x -> dep_on_anonymous_action x

  let build_alias_impl alias =
    let+ l =
      get_alias_definition alias
      >>= Memo.Build.parallel_map ~f:(fun (loc, definition) ->
              Memo.push_stack_frame
                (fun () ->
                  Action_builder.run (dep_on_alias_definition definition) Eager
                  >>| snd)
                ~human_readable_description:(fun () ->
                  Alias.describe alias ~loc))
    in
    Dep.Facts.group_paths_as_fact_files l

  module Pred = struct
    let build_impl g =
      let dir = File_selector.dir g in
      let* build_dir =
        is_target dir >>= function
        | false -> Memo.Build.return None
        | true -> build_dir dir
      in
      match build_dir with
      | None ->
        let* paths = Pred.eval g in
        let+ files =
          Memo.Build.parallel_map (Path.Set.to_list paths) ~f:(fun p ->
              let+ d = build_file p in
              (p, d))
        in
        Dep.Fact.Files.make
          ~files:(Path.Map.of_list_exn files)
          ~dirs:Path.Map.empty
      | Some (digest, path_map) ->
        let files =
          Path.Build.Map.foldi path_map ~init:Path.Map.empty
            ~f:(fun path digest acc ->
              let parent = Path.Build.parent_exn path |> Path.build in
              let path = Path.build path in
              match Path.equal parent dir && File_selector.test g path with
              | true -> Path.Map.add_exn acc path digest
              | false -> acc)
        in
        let dirs = Path.Map.singleton dir digest in
        Memo.Build.return (Dep.Fact.Files.make ~files ~dirs)

    (* CR-someday amokhov: This function is broken for [dir]s located inside a
       directory target. To check this and give a good error message we need to
       call [load_dir] on the parent directory but that creates a dependency
       cycle because of [copy_rules]. So, for now, this function just silently
       produces a wrong result (the glob evalutes to the empty set of files). Of
       course, we'd like to eventually fix this. *)
    let eval_impl g =
      let dir = File_selector.dir g in
      load_dir ~dir >>| function
      | Non_build targets -> Path.Set.filter targets ~f:(File_selector.test g)
      | Build { rules_here; _ } ->
        let only_generated_files = File_selector.only_generated_files g in
        (* We look only at [by_file_targets] because [File_selector] does not
           match directories. *)
        Path.Build.Map.foldi ~init:[] rules_here.by_file_targets
          ~f:(fun s { Rule.info; _ } acc ->
            match info with
            | Rule.Info.Source_file_copy _ when only_generated_files -> acc
            | _ ->
              let s = Path.build s in
              if File_selector.test g s then
                s :: acc
              else
                acc)
        |> Path.Set.of_list

    let eval_memo =
      Memo.create "eval-pred"
        ~human_readable_description:(fun glob ->
          Pp.concat
            [ Pp.textf "Evaluating predicate in directory %s"
                (Path.to_string_maybe_quoted (File_selector.dir glob))
            ])
        ~input:(module File_selector)
        ~cutoff:Path.Set.equal eval_impl

    let eval = Memo.exec eval_memo

    let build =
      Memo.exec
        (Memo.create "build-pred"
           ~input:(module File_selector)
           ~cutoff:Dep.Fact.Files.equal build_impl)
  end

  let build_file_memo =
    let cutoff =
      Tuple.T2.equal Digest.equal
        (Option.equal (Path.Build.Map.equal ~equal:Digest.equal))
    in
    Memo.create "build-file" ~input:(module Path) ~cutoff build_file_impl

  let build_file path = Memo.exec build_file_memo path >>| fst

  let build_dir path =
    let+ digest, path_map = Memo.exec build_file_memo path in
    match path_map with
    | Some path_map -> Some (digest, path_map)
    | None -> None

  let build_alias_memo =
    Memo.create "build-alias"
      ~input:(module Alias)
      ~cutoff:Dep.Fact.Files.equal build_alias_impl

  let build_alias = Memo.exec build_alias_memo

  let execute_rule_memo =
    Memo.create "execute-rule"
      ~input:(module Rule)
      (execute_rule_impl ~rule_kind:Normal_rule)

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Fdecl.set Rule_fn.loc_decl (fun () ->
        let+ stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            match
              Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo
            with
            | Some r -> Some (Rule.loc r)
            | None ->
              Option.bind
                (Memo.Stack_frame.as_instance_of frame
                   ~of_:execute_action_generic_stage2_memo) ~f:(fun x ->
                  x.action.Rule.Anonymous_action.loc)))
end

open Exported

type alias_definition = Rules.Dir_rules.Alias_spec.item

let dep_on_alias_definition = dep_on_alias_definition

let eval_pred = Pred.eval

let build_pred = Pred.build

let package_deps ~packages_of (pkg : Package.t) files =
  let rules_seen = ref Rule.Set.empty in
  let rec loop fn =
    match Path.as_in_build_dir fn with
    | None ->
      (* if this file isn't in the build dir, it doesn't belong to any package
         and it doesn't have dependencies that do *)
      Memo.Build.return Package.Id.Set.empty
    | Some fn ->
      let* pkgs = packages_of fn in
      if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs pkg.id then
        loop_deps fn
      else
        Memo.Build.return pkgs
  and loop_deps fn =
    get_rule (Path.build fn) >>= function
    | None -> Memo.Build.return Package.Id.Set.empty
    | Some rule ->
      if Rule.Set.mem !rules_seen rule then
        Memo.Build.return Package.Id.Set.empty
      else (
        rules_seen := Rule.Set.add !rules_seen rule;
        let* res = execute_rule rule in
        loop_files (Dep.Facts.paths res.deps |> Path.Map.keys)
      )
  and loop_files files =
    let+ sets = Memo.Build.parallel_map files ~f:loop in
    List.fold_left sets ~init:Package.Id.Set.empty ~f:Package.Id.Set.union
  in
  loop_files (Path.Set.to_list files)

let caused_by_cancellation (exn : Exn_with_backtrace.t) =
  match exn.exn with
  | Scheduler.Run.Build_cancelled -> true
  | Memo.Error.E err -> (
    match Memo.Error.get err with
    | Scheduler.Run.Build_cancelled -> true
    | _ -> false)
  | _ -> false

let report_early_exn exn =
  let t = t () in
  match caused_by_cancellation exn with
  | true -> Fiber.return ()
  | false ->
    let error = { Error.exn; id = Error.Id.gen () } in
    t.errors <- error :: t.errors;
    (match !Clflags.report_errors_config with
    | Early
    | Twice ->
      Dune_util.Report_error.report exn
    | Deterministic -> ());
    t.handler.error [ Add error ]

let handle_final_exns exns =
  match !Clflags.report_errors_config with
  | Early -> ()
  | Twice
  | Deterministic ->
    let report exn =
      if not (caused_by_cancellation exn) then Dune_util.Report_error.report exn
    in
    List.iter exns ~f:report

let run f =
  let open Fiber.O in
  Hooks.End_of_build.once Diff_promotion.finalize;
  let t = t () in
  let old_errors = t.errors in
  t.errors <- [];
  t.rule_done <- 0;
  t.rule_total <- 0;
  let* () =
    match old_errors with
    | [] -> Fiber.return ()
    | _ :: _ ->
      t.handler.error (List.map old_errors ~f:(fun x -> Handler.Remove x))
  in
  let f () =
    let* () = Handler.report_build_event t.handler Start in
    let* res =
      Fiber.collect_errors (fun () ->
          Memo.Build.run_with_error_handler f
            ~handle_error_no_raise:report_early_exn)
    in
    match res with
    | Ok res ->
      let+ () = Handler.report_build_event t.handler Finish in
      Ok res
    | Error exns ->
      handle_final_exns exns;
      let final_status =
        if List.exists exns ~f:caused_by_cancellation then
          Handler.Interrupt
        else
          Fail
      in
      let+ () = Handler.report_build_event t.handler final_status in
      Error `Already_reported
  in
  Fiber.Mutex.with_lock t.build_mutex f

let run_exn f =
  let open Fiber.O in
  let+ res = run f in
  match res with
  | Ok res -> res
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported

let build_file = build_file

let read_file p ~f =
  let+ _digest = build_file p in
  f p

let build_deps = build_deps

let file_exists = file_exists

let alias_exists = Load_rules.alias_exists

let execute_action = execute_action

let execute_action_stdout = execute_action_stdout

let load_dir_and_produce_its_rules ~dir =
  load_dir ~dir >>= function
  | Non_build _ -> Memo.Build.return ()
  | Build loaded -> Rules.produce loaded.rules_produced

let load_dir ~dir = load_dir_and_produce_its_rules ~dir

let init ~stats ~contexts ~promote_source ~cache_config ~cache_debug_flags
    ~sandboxing_preference ~rule_generator ~handler ~implicit_default_alias =
  let contexts =
    Memo.lazy_ ~name:"Build_system.init" (fun () ->
        let+ contexts = Memo.Lazy.force contexts in
        Context_name.Map.of_list_map_exn contexts ~f:(fun c ->
            (c.Build_context.name, c)))
  in
  let () =
    match (cache_config : Dune_cache.Config.t) with
    | Disabled -> ()
    | Enabled _ -> Dune_cache_storage.Layout.create_cache_directories ()
  in
  set
    { contexts
    ; rule_generator
    ; sandboxing_preference = sandboxing_preference @ Sandbox_mode.all
    ; rule_done = 0
    ; rule_total = 0
    ; errors = []
    ; handler = Option.value handler ~default:Handler.do_nothing
    ; (* This mutable table is safe: it merely maps paths to lazily created
         mutexes. *)
      locks = Table.create (module Path) 32
    ; promote_source
    ; build_mutex = Fiber.Mutex.create ()
    ; stats
    ; cache_config
    ; cache_debug_flags
    ; implicit_default_alias
    }

module Progress = struct
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }

  let complete t = t.number_of_rules_executed

  let remaining t = t.number_of_rules_discovered - t.number_of_rules_executed

  let is_determined { number_of_rules_discovered; number_of_rules_executed } =
    number_of_rules_discovered <> 0 || number_of_rules_executed <> 0
end

let get_current_progress () =
  let t = t () in
  { Progress.number_of_rules_executed = t.rule_done
  ; number_of_rules_discovered = t.rule_total
  }

let file_targets_of = file_targets_of

let all_targets () = all_targets (t ())

let last_event () = !Handler.last_event
