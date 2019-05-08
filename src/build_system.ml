open! Stdune
open Import
open Fiber.O

module Pre_rule = Rule

(* Where we store stamp files for aliases *)
let alias_dir = Path.(relative build_dir) ".aliases"

let () = Hooks.End_of_build.always Memo.reset

module Mkdir_p : sig
  val exec : Path.t -> unit
end = struct
  let def =
    Memo.create
      "mkdir_p"
      ~doc:"mkdir_p"
      ~input:(module Path)
      ~output:(Simple (module Unit))
      ~visibility:Hidden
      Sync
      (Some Path.mkdir_p)

  let exec p =
    if Path.is_managed p then
      Memo.exec def p
    else
      Exn.code_error "Mkdir_p.exec: attempted to create unmanaged dir"
        [ "p", Path.to_sexp p
        ]
end

module Promoted_to_delete : sig
  val add : Path.t -> unit
  val load : unit -> Path.Set.t
end = struct
  module P = Utils.Persistent(struct
      type t = Path.Set.t
      let name = "PROMOTED-TO-DELETE"
      let version = 1
    end)

  let db = ref Path.Set.empty

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  let needs_dumping = ref false

  let add p =
    if not (Path.Set.mem !db p) then begin
      needs_dumping := true;
      db := Path.Set.add !db p
    end

  let load () =
    Option.value ~default:Path.Set.empty (P.load fn)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then begin
      needs_dumping := false;
      load ()
      |> Path.Set.union !db
      |> P.dump fn
    end

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () =
  Promoted_to_delete.load ()

let rule_loc ~file_tree ~info ~dir =
  match (info : Rule.Info.t) with
  | From_dune_file loc -> loc
  | _ ->
    let dir = Path.drop_optional_build_context_src_exn dir in
    let file =
      match
        Option.bind (File_tree.find_dir file_tree dir)
          ~f:File_tree.Dir.dune_file
      with
      | Some file -> File_tree.Dune_file.path file
      | None      -> Path.Source.relative dir "_unknown_"
    in
    Loc.in_file (Path.source file)

module Internal_rule = struct
  module Id = struct
    include Id.Make ()
    module Top_closure_f = Top_closure.Make(Set)(struct
        type 'a t = 'a Fiber.t
        let return = Fiber.return
        let ( >>= ) = Fiber.O.( >>= )
      end)
    module Top_closure = Top_closure.Make(Set)(Monad.Id)
  end

  module T = struct
    type t =
      { id               : Id.t
      ; static_deps      : Static_deps.t Fiber.Once.t
      ; targets          : Path.Set.t
      ; context          : Context.t option
      ; build            : (unit, Action.t) Build.t
      ; mode             : Dune_file.Rule.Mode.t
      ; info             : Rule.Info.t
      ; dir              : Path.t
      ; env              : Env.t option
      ; sandbox          : bool
      ; locks            : Path.t list
      ; (* Reverse dependencies discovered so far, labelled by the
           requested target *)
        mutable rev_deps : (Path.t * t) list
      ; (* Transitive reverse dependencies discovered so far. *)
        mutable transitive_rev_deps : Id.Set.t
      }

    let compare a b = Id.compare a.id b.id
  end
  include T

  let _pp fmt { targets; dir ; _ } =
    Fmt.record fmt
      [ "targets", Fmt.const (Fmt.ocaml_list Path.pp) (Path.Set.to_list targets)
      ; "dir", Fmt.const Path.pp dir
      ]

  module Set = Set.Make(T)

  let equal a b = Id.equal a.id b.id
  let hash t = Id.hash t.id

  let to_sexp t : Sexp.t =
    Sexp.Encoder.record
      [ "id", Id.to_sexp t.id
      ; "loc", Sexp.Encoder.option Loc.to_sexp
                 (match t.info with
                  | From_dune_file loc -> Some loc
                  | _ -> None)
      ]

  let lib_deps t =
    (* Forcing this lazy ensures that the various globs and
       [if_file_exists] are resolved inside the [Build.t] value. *)
    let+ _ = Fiber.Once.get t.static_deps in
    Build.lib_deps t.build

  (* Represent the build goal given by the user. This rule is never
     actually executed and is only used starting point of all
     dependency paths. *)
  let root =
    { id          = Id.gen ()
    ; static_deps = Fiber.Once.create (fun () -> Fiber.return Static_deps.empty)
    ; targets     = Path.Set.empty
    ; context     = None
    ; build       = Build.return (Action.Progn [])
    ; mode        = Standard
    ; info        = Internal
    ; dir         = Path.root
    ; env         = None
    ; sandbox     = false
    ; locks       = []
    ; rev_deps    = []
    ; transitive_rev_deps = Id.Set.empty
    }

  (* Create a shim for the main build goal *)
  let shim_of_build_goal ~build ~static_deps =
    { root with
      id = Id.gen ()
    ; static_deps
    ; build
    }
end

module Alias0 = struct
  include Alias
  let dep t = Build.path (stamp_file t)

  let dep_multi_contexts ~dir ~name ~file_tree ~contexts =
    ignore
      (find_dir_specified_on_command_line ~dir ~file_tree : File_tree.Dir.t);
    Build.paths (List.map contexts ~f:(fun ctx ->
      let dir = Path.append_source (Path.(relative build_dir) ctx) dir in
      stamp_file (make ~dir name)))

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    Build.lazy_no_targets (lazy (
      File_tree.Dir.fold dir ~traverse_ignored_dirs:false
        ~init:(Build.return true)
        ~f:(fun dir acc ->
          let path = Path.append_source ctx_dir (File_tree.Dir.path dir) in
          let fn = stamp_file (make ~dir:path name) in
          acc
          >>>
          Build.if_file_exists fn
            ~then_:(Build.path fn >>^ Fn.const false)
            ~else_:(Build.arr Fn.id))))

  let dep_rec t ~loc ~file_tree =
    let ctx_dir, src_dir = Path.extract_build_context_dir_exn (Alias.dir t) in
    match File_tree.find_dir file_tree src_dir with
    | None ->
      Build.fail { fail = fun () ->
        Errors.fail loc "Don't know about directory %s!"
          (Path.Source.to_string_maybe_quoted src_dir) }
    | Some dir ->
      let name = Alias.name t in
      dep_rec_internal ~name ~dir ~ctx_dir
      >>^ fun is_empty ->
      if is_empty && not (is_standard name) then
        Errors.fail loc
          "This alias is empty.\n\
           Alias %S is not defined in %s or any of its descendants."
          name (Path.Source.to_string_maybe_quoted src_dir)

  let dep_rec_multi_contexts ~dir:src_dir ~name ~file_tree ~contexts =
    let open Build.O in
    let dir = find_dir_specified_on_command_line ~dir:src_dir ~file_tree in
    Build.all (List.map contexts ~f:(fun ctx ->
      let ctx_dir = Path.(relative build_dir) ctx in
      dep_rec_internal ~name ~dir ~ctx_dir))
    >>^ fun is_empty_list ->
    let is_empty = List.for_all is_empty_list ~f:Fn.id in
    if is_empty && not (is_standard name) then
      die "From the command line:\n\
           @{<error>Error@}: Alias %S is empty.\n\
           It is not defined in %s or any of its descendants."
        name (Path.Source.to_string_maybe_quoted src_dir)

  let package_install ~(context : Context.t) ~pkg =
    make (sprintf ".%s-files" (Package.Name.to_string pkg))
      ~dir:context.build_dir
end

module Loaded = struct
  type t = {
    rules_produced : Rules.t;
    targets : Path.Set.t;
  }
end

module Dir_status = struct

  type t =
    | Collecting_rules
    | Loaded  of Loaded.t
    | Forward of Path.t (* Load this directory first       *)
    | Failed_to_load
end

module Trace : sig
  module Entry : sig
    type t =
      { rule_digest    : Digest.t
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option
  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest    : Digest.t
      ; targets_digest : Digest.t
      }
  end

  (* Keyed by the first target *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  module P = Utils.Persistent(struct
      type nonrec t = t
      let name = "INCREMENTAL-DB"
      let version = 2
    end)

  let needs_dumping = ref false

  let t = lazy (
    match P.load file with
    | Some t -> t
    | None -> Path.Table.create 1024)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then begin
      needs_dumping := false;
      P.dump file (Lazy.force t)
    end

  let () = Hooks.End_of_build.always dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.replace t ~key:path ~data:e
end

type extra_sub_directories_to_keep =
  | All
  | These of String.Set.t

type hook =
  | Rule_started
  | Rule_completed

module Action_and_deps = struct
  type t = Action.t * Dep.Set.t

  let to_sexp (action, deps) =
    Sexp.Encoder.record
      [ "action", Dune_lang.to_sexp
                    (Action.For_shell.encode (Action.for_shell action))
      ; "deps", Dune_lang.to_sexp (Dep.Set.encode deps)
      ]
end

module Rule_fn = struct
  let loc_decl = Fdecl.create ()

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of string
    | Context of string

  let to_sexp = function
    | Install ctx -> Sexp.List [ Sexp.Atom "install"; Sexp.Atom ctx ]
    | Context s ->
      assert (not (s = "install"));
      Sexp.Atom s
end


type t =
  { (* File specification by targets *)
    files       : Internal_rule.t Path.Table.t
  ; contexts    : Context.t String.Map.t
  ; file_tree   : File_tree.t
  ; dirs : Dir_status.t Path.Table.t
  ; init_rules : Rules.t Fdecl.t
  ; gen_rules :
      (Context_or_install.t ->
       (dir:Path.t -> string list -> extra_sub_directories_to_keep) option) Fdecl.t
  ; mutable load_dir_stack : Path.t list
  ; (* Set of directories under _build that have at least one rule and
       all their ancestors. *)
    mutable build_dirs_to_keep : Path.Set.t
  ; hook : hook -> unit
  ; (* Package files are part of *)
    packages : (Path.t -> Package.Name.Set.t) Fdecl.t
  }

let t = ref None
let set x =
  match !t with
  | None -> t := Some x
  | Some _ -> Exn.code_error "build system already initialized" []
let get_build_system () =
  match !t with
  | Some t -> t
  | None -> Exn.code_error "build system not yet initialized" []
let reset () = t := None
let t = get_build_system

let string_of_paths set =
  Path.Set.to_list set
  |> List.map ~f:(fun p -> sprintf "- %s"
                             (Path.to_string_maybe_quoted
                                (Path.drop_optional_build_context p)))
  |> String.concat ~sep:"\n"

let string_of_source_paths set =
  Path.Source.Set.to_list set
  |> List.map ~f:Path.source
  |> Path.Set.of_list
  |> string_of_paths

let set_rule_generators ~init ~gen_rules =
  let t = t () in
  let ((), init_rules) = Rules.collect (fun () -> init ()) in
  Fdecl.set t.init_rules init_rules;
  Fdecl.set t.gen_rules gen_rules

let get_dir_status t ~dir =
  let just_started_collecting = ref false in
  let res =
    Path.Table.find_or_add t.dirs dir ~f:(fun _ ->
      match Path.as_in_source_tree dir with
      | Some dir ->
        Dir_status.Loaded {
          rules_produced = Rules.empty;
          targets =
            (
              Path.Source.Set.to_list (File_tree.files_of t.file_tree dir)
              |> List.map ~f:Path.source
              |> Path.Set.of_list);
        }
      | None ->
        if Path.equal dir Path.build_dir then
          (* Not allowed to look here *)
          Dir_status.Loaded
            { rules_produced = Rules.empty;
              targets = Path.Set.empty
            }
        else if not (Path.is_managed dir) then
          Dir_status.Loaded
            { rules_produced = Rules.empty;
              targets =
                match Path.readdir_unsorted dir with
                | Error Unix.ENOENT -> Path.Set.empty
                | Error m ->
                  Errors.warn Loc.none
                    "Unable to read %s@.Reason: %s@."
                    (Path.to_string_maybe_quoted dir)
                    (Unix.error_message m);
                  Path.Set.empty
                | Ok files ->
                  Path.Set.of_list (List.map files ~f:(Path.relative dir))
            }
        else begin
          let (ctx, sub_dir) = Path.extract_build_context_exn dir in
          if ctx = ".aliases" then
            Forward (Path.(append_source build_dir) sub_dir)
          else if ctx <> "install" && not (String.Map.mem t.contexts ctx) then
            Dir_status.Loaded {
              rules_produced = Rules.empty;
              targets = Path.Set.empty; }
          else
            (just_started_collecting := true;
             Collecting_rules)
        end)
  in
  res, !just_started_collecting

let add_spec t fn rule =
  match Path.Table.find t.files fn with
  | None ->
    Path.Table.add t.files fn rule
  | Some rule' ->
    let describe (rule : Internal_rule.t) =
      match rule.info with
      | From_dune_file { start; _ } ->
        start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
      | Internal -> "<internal location>"
      | Source_file_copy -> "file present in source tree"
    in
    die "Multiple rules generated for %s:\n\
         - %s\n\
         - %s%s"
      (Path.to_string_maybe_quoted fn)
      (describe rule')
      (describe rule)
      (match rule.info, rule'.info with
       | Source_file_copy, _ | _, Source_file_copy ->
         "\nHint: rm -f " ^ Path.to_string_maybe_quoted
                              (Path.drop_optional_build_context fn)
       | _ -> "")

(* This contains the targets of the actions that are being executed. On exit, we
   need to delete them as they might contain garbage *)
let pending_targets = ref Path.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
    let fns = !pending_targets in
    pending_targets := Path.Set.empty;
    Path.Set.iter fns ~f:Path.unlink_no_err)

let compute_targets_digest_after_rule_execution ~info targets =
  let good, bad =
    List.partition_map targets ~f:(fun fn ->
      match Utils.Cached_digest.refresh fn with
      | digest -> Left digest
      | exception (Unix.Unix_error _ | Sys_error _) -> Right fn)
  in
  match bad with
  | [] -> Digest.string (Marshal.to_string good [])
  | missing ->
    Errors.fail_opt
      (match (info : Rule.Info.t) with
       | From_dune_file loc -> Some loc
       | _ -> None)
      "rule failed to generate the following targets:\n%s"
      (string_of_paths (Path.Set.of_list missing))

let sandbox_dir = Path.relative Path.build_dir ".sandbox"

let locks : (Path.t, Fiber.Mutex.t) Hashtbl.t = Hashtbl.create 32

let rec with_locks mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Hashtbl.find_or_add locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks mutexes ~f)

let remove_old_artifacts t ~dir ~subdirs_to_keep =
  if not (Path.is_in_build_dir dir) ||
     Path.Table.mem t.files (Path.relative dir Config.dune_keep_fname) then
    ()
  else
    match Path.readdir_unsorted dir with
    | exception _ -> ()
    | Error _ -> ()
    | Ok files ->
      List.iter files ~f:(fun fn ->
        let path = Path.relative dir fn in
        let path_is_a_target = Path.Table.mem t.files path in
        if path_is_a_target then ()
        else
          match Unix.lstat (Path.to_string path) with
          | { st_kind = S_DIR; _ } -> begin
              match subdirs_to_keep with
              | All -> ()
              | These set ->
                if String.Set.mem set fn ||
                   Path.Set.mem t.build_dirs_to_keep path then
                  ()
                else
                  Path.rm_rf path
            end
          | exception _ -> Path.unlink path
          | _ -> Path.unlink path)

let no_rule_found =
  let fail fn ~loc =
    Errors.fail_opt loc "No rule found for %s" (Utils.describe_target fn)
  in
  fun t ~loc fn ->
    match Utils.analyse_target fn with
    | Other _ -> fail fn ~loc
    | Regular (ctx, _) ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        die "Trying to build %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))
    | Install (ctx, _) ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        die "Trying to build %s for install but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))
    | Alias (ctx, fn') ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        let fn = Path.append_source (Path.relative Path.build_dir ctx) fn' in
        die "Trying to build alias %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))

let fix_up_legacy_fallback_rules t ~file_tree_dir ~dir rules =
  (* Fix up non promote/fallback rules that have targets in the
     source tree if we are in a dune < 1.10 project *)
  match file_tree_dir with
  | None -> rules
  | Some ftdir ->
    let dune_version =
      Dune_project.dune_version (File_tree.Dir.project ftdir)
    in
    if Wp.t = Dune && dune_version >= (1, 10) then
      rules
    else begin
      let source_files =
        File_tree.Dir.files ftdir
        |> String.Set.to_list
        |> List.map ~f:(Path.relative dir)
        |> Path.Set.of_list
      in
      List.map rules ~f:(fun (rule : Pre_rule.t) ->
        match rule.mode with
        | Promote _ | Fallback | Ignore_source_files -> rule
        | Standard ->
          let inter = Path.Set.inter rule.targets source_files in
          if Path.Set.is_empty inter then
            rule
          else begin
            let mode, behavior =
              if Path.Set.equal inter rule.targets then
                (Dune_file.Rule.Mode.Fallback,
                 "acting as if the rule didn't exist")
              else
                (Dune_file.Rule.Mode.Promote
                   { lifetime = Unlimited
                   ; into = None
                   ; only =
                       Some
                         (Predicate_lang.of_pred
                            (fun s ->
                               Path.Set.mem inter (Path.relative dir s)))
                   },
                 "overwriting the source files with the generated one")
            in
            Errors.warn
              (rule_loc ~info:rule.info ~dir ~file_tree:t.file_tree)
              "The following files are both generated by a rule and are \
               present in\nthe source tree:@\n@[<v>%a@,@[%a@]@]"
              (Fmt.list (Fmt.prefix (Fmt.string "- ") Path.pp))
              (Path.Set.to_list inter
               |> List.map ~f:Path.drop_optional_build_context)
              Fmt.text
              (sprintf "Because %s, I am closing my eyes on this and \
                        I am %s. However, you should really delete \
                        these files from your source tree. I will no \
                        longer accept this once you upgrade your \
                        project to dune >= 1.10."
                 (match Wp.t with
                  | Jbuilder -> "you are using the jbuilder binary"
                  | Dune ->
                    "your project was written for dune " ^
                    Syntax.Version.to_string dune_version)
                 behavior);
            { rule with mode }
          end)
    end

(* +-----------------------------------------------------------------+
   | Adding rules to the system                                      |
   +-----------------------------------------------------------------+ *)

let rec add_build_dir_to_keep t ~dir =
  if not (Path.Set.mem t.build_dirs_to_keep dir) then begin
    t.build_dirs_to_keep <- Path.Set.add t.build_dirs_to_keep dir;
    Option.iter (Path.parent dir) ~f:(fun dir ->
      if not (Path.is_root dir) then
        add_build_dir_to_keep t ~dir)
  end

let handle_add_rule_effects f =
  let t = t () in
  let res, rules =
    Rules.collect f
  in
  (* CR-someday aalekseyev:
     find a way to do what [add_build_dir_to_keep] without relying
     on this side-effect so that memoization can be used here. *)
  Path.Build.Map.iteri (Rules.to_map rules)
    ~f:(fun dir _rules ->
      add_build_dir_to_keep t ~dir:(Path.build dir));
  res, rules

let rec compile_rule t pre_rule =
  let { Pre_rule.
        context
      ; env
      ; build
      ; targets
      ; sandbox
      ; mode
      ; locks
      ; info
      ; dir
      } =
    pre_rule
  in
  let static_deps = static_deps t build in
  let rule =
    let id = Internal_rule.Id.gen () in
    { Internal_rule.
      id
    ; static_deps
    ; targets
    ; build
    ; context
    ; env
    ; sandbox
    ; locks
    ; mode
    ; info
    ; dir
    ; transitive_rev_deps = Internal_rule.Id.Set.singleton id
    ; rev_deps = []
    }
  in
  Path.Set.iter targets ~f:(fun fn -> add_spec t fn rule)

and static_deps t build =
  Fiber.Once.create (fun () ->
    Fiber.return
      (Build.static_deps build
         ~all_targets:(targets_of t)))

and start_rule t _rule =
  t.hook Rule_started

and setup_copy_rules t ~ctx_dir ~non_target_source_files =
  Path.Source.Set.iter non_target_source_files ~f:(fun path ->
    let ctx_path = Path.append_source ctx_dir path in
    let build = Build.copy ~src:(Path.source path) ~dst:ctx_path in
    compile_rule t (Pre_rule.make build ~context:None ~env:None
                      ~info:Source_file_copy))

and load_dir   t ~dir = ignore (load_dir_and_get_targets t ~dir : Path.Set.t)
and load_dir_and_produce_its_rules t ~dir =
  let loaded = load_dir_and_get_rules_and_targets t ~dir in
  Rules.produce loaded.rules_produced

and targets_of t ~dir =         load_dir_and_get_targets t ~dir

and load_dir_and_get_targets t ~dir =
  (load_dir_and_get_rules_and_targets t ~dir).Loaded.targets

and load_dir_and_get_rules_and_targets t ~dir : Loaded.t =
  match get_dir_status t ~dir with
  | Failed_to_load, _ -> raise Already_reported

  | Loaded res, _ -> res

  | Forward dir', _ ->
    load_dir t ~dir:dir';
    begin match get_dir_status t ~dir with
    | Loaded res, _ -> res
    | _ -> assert false
    end

  | Collecting_rules, just_started ->
    let () =
      if just_started then ()
      else
        die "recursive dependency between directories:\n    %s"
          (String.concat ~sep:"\n--> "
             (List.map t.load_dir_stack ~f:Utils.describe_target))
    in

    t.load_dir_stack <- dir :: t.load_dir_stack;

    try
      load_dir_step2_exn t ~dir
    with exn ->
      (match Path.Table.find t.dirs dir with
       | Some (Loaded _) -> ()
       | _ ->
         (match t.load_dir_stack with
          | [] -> assert false
          | x :: l ->
            t.load_dir_stack <- l;
            assert (Path.equal x dir)));
      Path.Table.replace t.dirs ~key:dir ~data:Failed_to_load;
      reraise exn

and load_dir_step2_exn t ~dir =
  let context_name, sub_dir = match Utils.analyse_target dir with
    | Install (ctx, path) ->
      Context_or_install.Install ctx, path
    | Regular (ctx, path) ->
      Context_or_install.Context ctx, path
    | Alias _ | Other _ ->
      Exn.code_error "[load_dir_step2_exn] was called on a strange path"
        ["path", (Path.to_sexp dir)]
  in

  (* Load all the rules *)
  let extra_subdirs_to_keep, rules_produced =
    let gen_rules =
      match (Fdecl.get t.gen_rules) context_name with
      | None ->
        Exn.code_error "[gen_rules] did not specify rules for the context"
          ["context_name", (Context_or_install.to_sexp context_name)]
      | Some rules ->
        rules
    in
    handle_add_rule_effects
      (fun () -> gen_rules ~dir (Path.Source.explode sub_dir))
  in
  let rules =
    Rules.Dir_rules.union
      (Rules.find rules_produced dir)
      (Rules.find (Fdecl.get t.init_rules) dir)
  in
  let collected = Rules.Dir_rules.consume rules in

  (* Compute alias rules *)
  let alias_dir, alias_rules =
    match context_name with
    | Context context_name ->
      let alias_dir = Path.append_source (Path.relative alias_dir context_name) sub_dir in
      let alias_rules, alias_stamp_files =
        let open Build.O in
        let aliases =
          collected.aliases
        in
        let aliases =
          if String.Map.mem aliases "default" then
            aliases
          else
            match Path.extract_build_context_dir dir with
            | None -> aliases
            | Some (ctx_dir, src_dir) ->
              match File_tree.find_dir t.file_tree src_dir with
              | None -> aliases
              | Some dir ->
                String.Map.add aliases "default"
                  { deps = Path.Set.empty
                  ; dyn_deps =
                      (Alias0.dep_rec_internal ~name:"install" ~dir ~ctx_dir
                       >>^ fun (_ : bool) ->
                       Path.Set.empty)
                  ; actions = Appendable_list.empty
                  }
        in
        String.Map.foldi aliases ~init:([], Path.Set.empty)
          ~f:(fun name
               { Rules.Dir_rules.Alias_spec. deps; dyn_deps; actions }
               (rules, alias_stamp_files) ->
            let base_path = Path.relative alias_dir name in
            let rules, action_stamp_files =
              List.fold_left
                (Appendable_list.to_list actions)
                ~init:(rules, Path.Set.empty)
                ~f:(fun (rules, action_stamp_files)
                     { Rules.Dir_rules.
                       stamp; action; locks ; context ; loc ; env } ->
                     let path =
                       Path.extend_basename base_path
                         ~suffix:("-" ^ Digest.to_string stamp)
                     in
                     let rule =
                       Pre_rule.make ~locks ~context:(Some context) ~env
                         ~info:(Rule.Info.of_loc_opt loc)
                         (Build.progn [ action; Build.create_file path ])
                     in
                     (rule :: rules, Path.Set.add action_stamp_files path))
            in
            let deps = Path.Set.union deps action_stamp_files in
            let path = Path.extend_basename base_path ~suffix:Alias0.suffix in
            let targets =
              Path.Set.add action_stamp_files path
              |> Path.Set.union alias_stamp_files
            in
            (Pre_rule.make
               ~context:None
               ~env:None
               (Build.path_set deps >>>
                dyn_deps >>>
                Build.dyn_path_set (Build.arr Fn.id)
                >>^ (fun dyn_deps ->
                  let deps = Path.Set.union deps dyn_deps in
                  Action.with_stdout_to path
                    (Action.digest_files (Path.Set.to_list deps)))
                >>>
                Build.action_dyn () ~targets:[path])
             :: rules,
             targets))
      in
      Path.Table.replace t.dirs ~key:alias_dir
        ~data:(Loaded {
          rules_produced = Rules.empty;
          targets = alias_stamp_files });
      Some alias_dir, alias_rules
    | Install _ ->
      None, []
  in

  let file_tree_dir =
    match context_name with
    | Install _ ->
      None
    | Context _ ->
      File_tree.find_dir t.file_tree sub_dir
  in

  let rules =
    fix_up_legacy_fallback_rules t ~file_tree_dir ~dir collected.rules
  in

  (* Compute the set of targets and the set of source files that must
     not be copied *)
  let user_rule_targets, source_files_to_ignore =
    List.fold_left rules ~init:(Path.Set.empty, Path.Set.empty)
      ~f:(fun (acc_targets, acc_ignored) { Pre_rule.targets; mode; _ } ->
        (Path.Set.union targets acc_targets,
         match mode with
         | Promote { only = None; _ } | Ignore_source_files ->
           Path.Set.union targets acc_ignored
         | Promote { only = Some pred; _ } ->
           let to_ignore =
             Path.Set.filter targets ~f:(fun target ->
               Predicate_lang.exec pred (Path.reach target ~from:dir)
                 ~standard:Predicate_lang.true_)
           in
           Path.Set.union to_ignore acc_ignored
         | _ ->
           acc_ignored))
  in
  let source_files_to_ignore =
    Path.Set.to_list source_files_to_ignore
    |> List.map ~f:Path.drop_build_context_exn
    |> Path.Source.Set.of_list
  in
  (* Take into account the source files *)
  let targets, to_copy, subdirs_to_keep =
    match context_name with
    | Install _ ->
      (user_rule_targets,
       None,
       String.Set.empty)
    | Context context_name ->
      (* This condition is [true] because of [get_dir_status] *)
      assert (String.Map.mem t.contexts context_name);
      let files, subdirs =
        match file_tree_dir with
        | None -> (Path.Source.Set.empty, String.Set.empty)
        | Some dir ->
          (File_tree.Dir.file_paths    dir,
           File_tree.Dir.sub_dir_names dir)
      in
      let files = Path.Source.Set.diff files source_files_to_ignore in
      if Path.Source.Set.is_empty files then
        (user_rule_targets, None, subdirs)
      else
        let ctx_path = Path.(relative build_dir) context_name in
        (Path.Set.union user_rule_targets
           (Path.Source.Set.to_list files
            |> List.map ~f:(Path.append_source ctx_path)
            |> Path.Set.of_list
           ),
         Some (ctx_path, files),
         subdirs)
  in
  let subdirs_to_keep =
    match extra_subdirs_to_keep with
    | All -> All
    | These set -> These (String.Set.union subdirs_to_keep set)
  in

  (* Filter out fallback rules *)
  let rules =
    match to_copy with
    | None ->
      (* If there are no source files to copy, fallback rules are
         automatically kept *)
      rules
    | Some (_, to_copy) ->
      List.filter rules ~f:(fun (rule : Pre_rule.t) ->
        match rule.mode with
        | Standard | Promote _ | Ignore_source_files -> true
        | Fallback ->
          let source_files_for_targtes =
            (* All targets are in [dir] and we know it correspond to a
               directory of a build context since there are source
               files to copy, so this call can't fail. *)
            Path.Set.to_list rule.targets
            |> List.map ~f:Path.drop_build_context_exn
            |> Path.Source.Set.of_list
          in
          if Path.Source.Set.is_subset source_files_for_targtes ~of_:to_copy then
            (* All targets are present *)
            false
          else begin
            if Path.Source.Set.is_empty
                 (Path.Source.Set.inter source_files_for_targtes to_copy) then
              (* No target is present *)
              true
            else begin
              let absent_targets =
                Path.Source.Set.diff source_files_for_targtes to_copy
              in
              let present_targets =
                Path.Source.Set.diff source_files_for_targtes absent_targets
              in
              Errors.fail
                (rule_loc
                   ~file_tree:t.file_tree
                   ~info:rule.info
                   ~dir:(Path.drop_optional_build_context dir))
                "\
Some of the targets of this fallback rule are present in the source tree,
and some are not. This is not allowed. Either none of the targets must
be present in the source tree, either they must all be.

The following targets are present:
%s

The following targets are not:
%s
"
                (string_of_source_paths present_targets)
                (string_of_source_paths absent_targets)
            end
          end)
  in

  (* Set the directory status to loaded *)
  Path.Table.replace t.dirs ~key:dir ~data:(Loaded {
    rules_produced;
    targets });
  (match t.load_dir_stack with
   | [] -> assert false
   | x :: l ->
     t.load_dir_stack <- l;
     assert (Path.equal x dir));

  (* Compile the rules and cleanup stale artifacts *)
  List.iter rules ~f:(compile_rule t);
  Option.iter to_copy ~f:(fun (ctx_dir, source_files) ->
    setup_copy_rules t ~ctx_dir ~non_target_source_files:source_files);
  remove_old_artifacts t ~dir ~subdirs_to_keep;

  List.iter alias_rules ~f:(compile_rule t);
  Option.iter alias_dir ~f:(fun alias_dir ->
    remove_old_artifacts t ~dir:alias_dir ~subdirs_to_keep);

  { rules_produced; targets }

let get_rule_other t fn =
  let dir = Path.parent_exn fn in
  if Path.is_in_build_dir dir then
    load_dir t ~dir;
  Fiber.return (Path.Table.find t.files fn)

and get_rule t path =
  match Path.Table.find t.files path with
  | Some _ as some -> Fiber.return some
  | None ->
    let dir = Path.parent_exn path in
    if Path.is_strict_descendant_of_build_dir dir then begin
      load_dir t ~dir;
      match Path.Table.find t.files path with
      | Some _ as some -> Fiber.return some
      | None ->
        let loc = Rule_fn.loc () in
        no_rule_found t ~loc path
    end else if Path.exists path then
      Fiber.return None
    else
      let loc = Rule_fn.loc () in
      Errors.fail_opt loc
        "File unavailable: %s" (Path.to_string_maybe_quoted path)

let all_targets () =
  let t = t () in
  String.Map.iter t.contexts ~f:(fun ctx ->
    File_tree.fold t.file_tree ~traverse_ignored_dirs:true ~init:()
      ~f:(fun dir () ->
        load_dir t
          ~dir:(Path.append_source ctx.Context.build_dir (File_tree.Dir.path dir))));
  Path.Table.foldi t.files ~init:[] ~f:(fun key _ acc -> key :: acc)

let build_file_def =
  Memo.create
    "build-file"
    ~output:(Allow_cutoff (module Unit))
    ~doc:"Build a file."
    ~input:(module Path)
    ~visibility:(Public Path_dune_lang.decode)
    Async
    None

let build_file = Memo.exec build_file_def

let execute_rule_def =
  Memo.create
    "execute-rule"
    ~output:(Allow_cutoff (module Unit))
    ~doc:"-"
    ~input:(module Internal_rule)
    ~visibility:Hidden
    Async
    None

module Pred = struct
  let eval_def =
    Memo.create "eval-pred"
      ~doc:"Evaluate a predicate in a directory"
      ~input:(module File_selector)
      ~output:(Allow_cutoff (module Path.Set))
      ~visibility:Hidden
      Sync
      None

  let build_def =
    Memo.create "build-pred"
      ~doc:"build a predicate"
      ~input:(module File_selector)
      ~output:(Allow_cutoff (module Unit))
      ~visibility:Hidden
      Async
      None
end

let eval_pred g = Memo.exec Pred.eval_def g

let execute_rule = Memo.exec execute_rule_def

let build_pred g = Memo.exec Pred.build_def g

let build_deps =
  Dep.Set.parallel_iter ~f:(function
    | Alias a -> build_file (Alias.stamp_file a)
    | File f -> build_file f
    | Glob g -> build_pred g
    | Universe
    | Env _ -> Fiber.return ())

(* Evaluate a rule and return the action and set of dynamic dependencies *)
let evaluate_action_and_dynamic_deps_def =
  let f (rule : Internal_rule.t) =
    let* static_deps = Fiber.Once.get rule.static_deps in
    let rule_deps = Static_deps.rule_deps static_deps in
    let+ () = build_deps rule_deps in
    Build.exec ~eval_pred rule.build ()
  in
  Memo.create
    "evaluate-action-and-dynamic-deps"
    ~output:(Simple (module Action_and_deps))
    ~doc:"Evaluate the build arrow part of a rule and return the \
          action and dynamic dependency of the rule."
    ~input:(module Internal_rule)
    ~visibility:Hidden
    Async
    (Some f)

let evaluate_action_and_dynamic_deps =
  Memo.exec evaluate_action_and_dynamic_deps_def

let () =
  Fdecl.set Rule_fn.loc_decl (fun () ->
    let stack = Memo.get_call_stack () in
    List.find_map stack ~f:(fun frame ->
      match Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_def with
      | Some input -> Some input
      | None ->
        Memo.Stack_frame.as_instance_of frame ~of_:evaluate_action_and_dynamic_deps_def)
    |> Option.bind ~f:(fun rule ->
      match rule.Internal_rule.info with
      | From_dune_file loc -> Some loc
      | _ -> None))

let evaluate_rule (rule : Internal_rule.t) =
  let* static_deps = Fiber.Once.get rule.static_deps in
  let+ (action, dynamic_action_deps) = evaluate_action_and_dynamic_deps rule in
  let static_action_deps = Static_deps.action_deps static_deps in
  let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
  (action, action_deps)

(* Same as the function just bellow, but with less opportunity for
   parallelism. We keep this dead code here for documentation purposes
   as it is easier to read the one bellow. The reader only has to
   check that both function do the same thing. *)
let _evaluate_rule_and_wait_for_dependencies rule =
  let* (action, action_deps) = evaluate_rule rule in
  let+ () = build_deps action_deps in
  (action, action_deps)

(* The following function does exactly the same as the function above
   with the difference that it starts the build of static dependencies
   before we know the final action and set of dynamic dependencies. We
   do this to increase opportunities for parallelism.
*)
let evaluate_rule_and_wait_for_dependencies (rule : Internal_rule.t) =
  let* static_deps = Fiber.Once.get rule.static_deps in
  let static_action_deps = Static_deps.action_deps static_deps in
  (* Build the static dependencies in parallel with evaluation the
     action and dynamic dependencies *)
  let* (action, dynamic_action_deps) =
    Fiber.fork_and_join_unit
      (fun () -> build_deps static_action_deps)
      (fun () -> evaluate_action_and_dynamic_deps rule)
  in
  build_deps dynamic_action_deps
  >>>
  let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
  Fiber.return (action, action_deps)

let () =
  (* Evaluate and execute a rule *)
  let execute_rule rule =
    let t = t () in
    let { Internal_rule.
          dir
        ; targets
        ; env
        ; context
        ; mode
        ; sandbox
        ; locks
        ; id = _
        ; static_deps = _
        ; build = _
        ; info
        ; transitive_rev_deps = _
        ; rev_deps = _
        } = rule
    in
    start_rule t rule;
    let* (action, deps) = evaluate_rule_and_wait_for_dependencies rule in
    Mkdir_p.exec dir;
    let targets_as_list  = Path.Set.to_list targets  in
    let head_target = List.hd targets_as_list in
    let prev_trace = Trace.get head_target in
    let rule_digest =
      let env =
        match env, context with
        | None, None -> Env.initial
        | Some e, _ -> e
        | None, Some c -> c.env
      in
      let trace =
        ( Dep.Set.trace deps ~env ~eval_pred
        , List.map targets_as_list ~f:Path.to_string
        , Option.map context ~f:(fun c -> c.name)
        , Action.for_shell action
        )
      in
      Digest.string (Marshal.to_string trace [])
    in
    let targets_digest =
      match List.map targets_as_list ~f:Utils.Cached_digest.file with
      | l -> Some (Digest.string (Marshal.to_string l []))
      | exception (Unix.Unix_error _ | Sys_error _) -> None
    in
    let sandbox_dir =
      if sandbox then
        let digest = Digest.to_string rule_digest in
        Some (Path.relative sandbox_dir digest)
      else
        None
    in
    let force =
      !Clflags.force &&
      List.exists targets_as_list ~f:Path.is_alias_stamp_file
    in
    let something_changed =
      match prev_trace, targets_digest, Dep.Set.has_universe deps with
      | Some prev_trace, Some targets_digest, false ->
        prev_trace.rule_digest <> rule_digest ||
        prev_trace.targets_digest <> targets_digest
      | _ -> true
    in
    begin
      if force || something_changed then begin
        List.iter targets_as_list ~f:Path.unlink_no_err;
        pending_targets := Path.Set.union targets !pending_targets;
        let action =
          match sandbox_dir with
          | None ->
            action
          | Some sandbox_dir ->
            Path.rm_rf sandbox_dir;
            let sandboxed path = Path.sandbox_managed_paths ~sandbox_dir path in
            Dep.Set.dirs deps
            |> Path.Set.iter ~f:(fun p ->
              let p = sandboxed p in
              if Path.is_managed p then
                Mkdir_p.exec p);
            Mkdir_p.exec (sandboxed dir);
            Action.sandbox action
              ~sandboxed
              ~deps
              ~targets:targets_as_list
              ~eval_pred
        in
        Path.Set.iter (Action.chdirs action) ~f:Mkdir_p.exec;
        let+ () =
          with_locks locks ~f:(fun () ->
            Action_exec.exec ~context ~env ~targets action)
        in
        Option.iter sandbox_dir ~f:Path.rm_rf;
        (* All went well, these targets are no longer pending *)
        pending_targets := Path.Set.diff !pending_targets targets;
        let targets_digest =
          compute_targets_digest_after_rule_execution ~info targets_as_list
        in
        Trace.set head_target { rule_digest; targets_digest }
      end else
        Fiber.return ()
    end >>| fun () ->
    begin
      match mode with
      | Standard | Fallback | Ignore_source_files -> ()
      | Promote { lifetime; into; only } ->
        Path.Set.iter targets ~f:(fun path ->
          let consider_for_promotion =
            match only with
            | None -> true
            | Some pred ->
              Predicate_lang.exec pred (Path.reach path ~from:dir)
                ~standard:Predicate_lang.true_
          in
          if consider_for_promotion then begin
            let in_source_tree = Path.drop_build_context_exn path in
            let in_source_tree =
              match into with
              | None -> in_source_tree
              | Some { loc; dir } ->
                Path.Source.relative
                  (Path.Source.relative (Path.Source.parent_exn in_source_tree) dir
                     ~error_loc:loc)
                  (Path.Source.basename in_source_tree)
            in
            if not (Path.exists (Path.source in_source_tree)) ||
               (Utils.Cached_digest.file path <>
                Utils.Cached_digest.file (Path.source in_source_tree)) then begin
              if lifetime = Until_clean then
                Promoted_to_delete.add (Path.source in_source_tree);
              Scheduler.ignore_for_watch (Path.source in_source_tree);
              Io.copy_file ~src:path ~dst:(Path.source in_source_tree) ()
            end
          end)
    end;
    t.hook Rule_completed
  in
  Memo.set_impl execute_rule_def execute_rule

let () =
  (* a rule can have multiple files, but rule.run_rule may only be called once *)
  let build_file path =
    let t = t () in
    let on_error exn = Dep_path.reraise exn (Path path) in
    Fiber.with_error_handler ~on_error (fun () ->
      get_rule t path >>= function
      | None ->
        (* file already exists *)
        Fiber.return ()
      | Some rule -> execute_rule rule)
  in
  Memo.set_impl build_file_def build_file

let () =
  let f g =
    eval_pred g
    |> Path.Set.to_list
    |> Fiber.parallel_iter ~f:build_file
  in
  Memo.set_impl Pred.build_def f

let shim_of_build_goal t request =
  let request =
    let open Build.O in
    request >>^ fun () ->
    Action.Progn []
  in
  Internal_rule.shim_of_build_goal
    ~build:request
    ~static_deps:(static_deps t request)

let build_request t ~request =
  let result = Fdecl.create () in
  let request =
    let open Build.O in
    request >>^ fun res ->
    Fdecl.set result res
  in
  let rule = shim_of_build_goal t request in
  let+ (_act, _deps) = evaluate_rule_and_wait_for_dependencies rule in
  Fdecl.get result

let process_memcycle exn =
  let cycle =
    Memo.Cycle_error.get exn
    |> List.filter_map ~f:(Memo.Stack_frame.as_instance_of ~of_:build_file_def)
  in
  match List.last cycle with
  | None ->
    let frames : string list =
      Memo.Cycle_error.get exn
      |> List.map ~f:(Format.asprintf "%a" Memo.Stack_frame.pp)
    in
    Exn.code_error "dependency cycle that does not involve any files"
      ["frames", Sexp.Encoder.(list string) frames]
  | Some last ->
    let first = List.hd cycle in
    let cycle = if last = first then cycle else last :: cycle in
    Exn.Fatal_error
      (Format.asprintf "Dependency cycle between the following files:\n    %s"
         (List.map cycle ~f:Path.to_string_maybe_quoted
          |> String.concat ~sep:"\n--> "))

let do_build ~request =
  let t = t () in
  Hooks.End_of_build.once Promotion.finalize;
  (fun () -> build_request t ~request)
  |> Fiber.with_error_handler ~on_error:(
    Exn_with_backtrace.map_and_reraise
      ~f:(Dep_path.map ~f:(function
        | Memo.Cycle_error.E exn -> process_memcycle exn
        | _ as exn -> exn
      )))

let init ~contexts ~file_tree ~hook =
  let contexts =
    List.map contexts ~f:(fun c -> (c.Context.name, c))
    |> String.Map.of_list_exn
  in
  set
    { contexts
    ; files      = Path.Table.create 1024
    ; packages   = Fdecl.create ()
    ; dirs       = Path.Table.create 1024
    ; load_dir_stack = []
    ; file_tree
    ; gen_rules = Fdecl.create ()
    ; init_rules = Fdecl.create ()
    ; build_dirs_to_keep = Path.Set.empty
    ; hook
    }

module Rule = struct
  module Id = Internal_rule.Id

  type t =
    { id      : Id.t
    ; dir     : Path.t
    ; deps    : Dep.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }

  let compare a b = Id.compare a.id b.id

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
end

let set_packages f =
  let t = t () in
  Fdecl.set t.packages f

let package_deps pkg files =
  let t = t () in
  let rules_seen = ref Internal_rule.Set.empty in
  let rec loop fn acc =
    let pkgs = Fdecl.get t.packages fn in
    match Package.Name.Set.is_empty pkgs with
    | true -> loop_deps fn acc
    | false ->
      if Package.Name.Set.mem pkgs pkg then
        loop_deps fn acc
      else
        Package.Name.Set.union acc pkgs
  and loop_deps fn acc =
    match Path.Table.find t.files fn with
    | None -> acc
    | Some ir ->
      if Internal_rule.Set.mem !rules_seen ir then
        acc
      else begin
        rules_seen := Internal_rule.Set.add !rules_seen ir;
        (* We know that at this point of execution, all the relevant
           ivars have been filled so the following calls to
           [X.peek_exn] cannot raise. *)
        let static_deps = Fiber.Once.peek_exn ir.static_deps in
        let static_action_deps = Static_deps.action_deps static_deps in
        let _act, dynamic_action_deps =
          Memo.peek_exn evaluate_action_and_dynamic_deps_def ir
        in
        let action_deps =
          Path.Set.union
            (Dep.Set.paths static_action_deps ~eval_pred)
            (Dep.Set.paths dynamic_action_deps ~eval_pred)
        in
        Path.Set.fold action_deps ~init:acc ~f:loop
      end
  in
  let open Build.O in
  Build.paths_for_rule files >>^ fun () ->
  (* We know that at this point of execution, all the relevant ivars
     have been filled *)
  Path.Set.fold files ~init:Package.Name.Set.empty ~f:loop_deps

let prefix_rules prefix ~f =
  let targets = Build.targets prefix in
  if not (Path.Set.is_empty targets) then
    Exn.code_error "Build_system.prefix_rules' prefix contains targets"
      ["targets", Path.Set.to_sexp targets];
  let res, rules = Rules.collect f in
  Rules.produce (Rules.map_rules rules ~f:(fun rule ->
    { rule with build = Build.O.(>>>) prefix rule.build }));
  res

module Alias = Alias0

let () =
  let f g =
    let dir = File_selector.dir g in
    let t = t () in
    Path.Set.filter (targets_of t ~dir) ~f:(File_selector.test g)
  in
  Memo.set_impl Pred.eval_def f

let targets_of ~dir = targets_of (t ()) ~dir
let load_dir ~dir = load_dir (t ()) ~dir

let is_target file =
  Path.Set.mem (targets_of ~dir:(Path.parent_exn file)) file

module Print_rules : sig
  val evaluate_rules
    :  recursive:bool
    -> request:(unit, unit) Build.t
    -> Rule.t list Fiber.t
end = struct
  let rules_for_files rules deps =
    Dep.Set.paths deps ~eval_pred
    |> Path.Set.fold ~init:Rule.Set.empty ~f:(fun path acc ->
      match Path.Map.find rules path with
      | None -> acc
      | Some rule -> Rule.Set.add acc rule)
    |> Rule.Set.to_list

  let entry_point t ~f =
    (match t.load_dir_stack with
     | [] ->
       ()
     | stack ->
       Exn.code_error
         "Build_system.entry_point: called inside the rule generator callback"
         ["stack", Sexp.Encoder.list Path.to_sexp stack]
    );
    f ()

  let evaluate_rules ~recursive ~request =
    let t = t () in
    entry_point t ~f:(fun () ->
      let rules = ref Internal_rule.Id.Map.empty in
      let rec run_rule (rule : Internal_rule.t) =
        if Internal_rule.Id.Map.mem !rules rule.id then
          Fiber.return ()
        else begin
          let* (action, deps) = evaluate_rule rule in
          let rule =
            { Rule.
              id = rule.id
            ; dir = rule.dir
            ; deps
            ; targets = rule.targets
            ; context = rule.context
            ; action
            } in
          rules := Internal_rule.Id.Map.add !rules rule.id rule;
          if recursive then
            Dep.Set.parallel_iter_files deps ~f:proc_rule ~eval_pred
          else
            Fiber.return ()
        end
      and proc_rule dep =
        get_rule_other t dep >>= function
        | None -> Fiber.return () (* external files *)
        | Some rule -> run_rule rule
      in
      let rule_shim = shim_of_build_goal t request in
      let* (_act, goal) = evaluate_rule rule_shim in
      let+ () = Dep.Set.parallel_iter_files goal ~f:proc_rule ~eval_pred in
      let rules =
        Internal_rule.Id.Map.fold !rules ~init:Path.Map.empty
          ~f:(fun (r : Rule.t) acc ->
            Path.Set.fold r.targets ~init:acc ~f:(fun fn acc ->
              Path.Map.add acc fn r)) in
      match
        Rule.Id.Top_closure.top_closure
          (rules_for_files rules goal)
          ~key:(fun (r : Rule.t) -> r.id)
          ~deps:(fun (r : Rule.t) -> rules_for_files rules r.deps)
      with
      | Ok l -> l
      | Error cycle ->
        die "dependency cycle detected:\n   %s"
          (List.map cycle ~f:(fun rule ->
             Path.to_string (Path.Set.choose_exn rule.Rule.targets))
           |> String.concat ~sep:"\n-> "))
end

include Print_rules

module All_lib_deps : sig
  val all_lib_deps
    :  request:(unit, unit) Build.t
    -> Lib_deps_info.t Path.Source.Map.t String.Map.t Fiber.t
end = struct
  let static_deps_of_request request =
    Static_deps.paths @@
    Build.static_deps request ~all_targets:targets_of

  let rules_for_files t paths =
    Path.Set.fold paths ~init:[] ~f:(fun path acc ->
      if Path.is_in_build_dir path then
        load_dir ~dir:(Path.parent_exn path);
      match Path.Table.find t.files path with
      | None -> acc
      | Some rule -> rule :: acc)
    |> Internal_rule.Set.of_list
    |> Internal_rule.Set.to_list

  let rules_for_targets t targets =
    Internal_rule.Id.Top_closure_f.top_closure
      (rules_for_files t targets)
      ~key:(fun (r : Internal_rule.t) -> r.id)
      ~deps:(fun (r : Internal_rule.t) ->
        Fiber.Once.get r.static_deps
        >>| Static_deps.paths ~eval_pred
        >>| rules_for_files t)
    >>| function
    | Ok l -> l
    | Error cycle ->
      die "dependency cycle detected:\n   %s"
        (List.map cycle ~f:(fun rule ->
           Path.to_string (Path.Set.choose_exn rule.Internal_rule.targets))
         |> String.concat ~sep:"\n-> ")

  let all_lib_deps ~request =
    let t = t () in
    let targets = static_deps_of_request request ~eval_pred in
    let* rules= rules_for_targets t targets in
    let+ lib_deps =
      Fiber.parallel_map rules ~f:(fun rule ->
        let+ deps = Internal_rule.lib_deps rule in
        (rule, deps))
    in
    List.fold_left lib_deps ~init:[]
      ~f:(fun acc (rule, deps) ->
        if Lib_name.Map.is_empty deps then
          acc
        else
          match Path.extract_build_context rule.Internal_rule.dir with
          | None -> acc
          | Some (context, p) -> ((context, (p, deps)) :: acc))
    |> String.Map.of_list_multi
    |> String.Map.filteri ~f:(fun ctx _ -> String.Map.mem t.contexts ctx)
    |> String.Map.map ~f:(Path.Source.Map.of_list_reduce ~f:Lib_deps_info.merge)
end

include All_lib_deps

let load_dir ~dir = load_dir_and_produce_its_rules (t ()) ~dir
