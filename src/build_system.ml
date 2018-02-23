open Import
open Fiber.O

module Pset  = Path.Set
module Pmap  = Path.Map
module Vspec = Build.Vspec

(* Where we store stamp files for aliases *)
let alias_dir = Path.(relative build_dir) ".aliases"

(* Where we store stamp files for [stamp_file_for_files_of] *)
let misc_dir = Path.(relative build_dir) ".misc"

module Promoted_to_delete = struct
  let db = ref []

  let fn = "_build/.to-delete-in-source-tree"

  let add p = db := p :: !db

  let load () =
    if Sys.file_exists fn then
      Sexp.load ~fname:fn ~mode:Many
      |> List.map ~f:Path.t
    else
      []

  let dump () =
    let db = Pset.union (Pset.of_list !db) (Pset.of_list (load ())) in
    if Sys.file_exists "_build" then
      Io.write_file fn
        (String.concat ~sep:""
           (List.map (Pset.to_list db) ~f:(fun p ->
              Sexp.to_string (Path.sexp_of_t p) ^ "\n")))
end

let files_in_source_tree_to_delete () =
  Promoted_to_delete.load ()

module Dependency_path = struct
  type rule_info =
    { (* Reason why this rule was visited *)
      requested_file : Path.t
    ; (* All targets of the rule *)
      targets        : Pset.t
    }

  type t =
    { dependency_path : rule_info list
    ; (* Union of all [targets] fields in [dependency_path]. Depending on any of these
         means that there is a cycle. *)
      targets_seen    : Pset.t
    }

  let var = Fiber.Var.create ()

  let empty =
    { dependency_path = []
    ; targets_seen    = Pset.empty
    }

  let dependency_cycle last dep_path =
    let rec build_loop acc dep_path =
      match dep_path with
      | [] -> assert false
      |  { requested_file; targets } :: dep_path ->
        if Pset.mem targets last then
          last :: acc
        else
          build_loop (requested_file :: acc) dep_path
    in
    let loop = build_loop [last] dep_path in
    die "Dependency cycle between the following files:\n    %s"
      (String.concat ~sep:"\n--> "
         (List.map loop ~f:Path.to_string))

  let push requested_file ~targets ~f =
    Fiber.Var.get var >>= fun x ->
    let t = Option.value x ~default:empty in
    if Pset.mem t.targets_seen requested_file then
      dependency_cycle requested_file t.dependency_path;
    let dependency_path =
      { requested_file; targets } :: t.dependency_path
    in
    let t =
      { dependency_path
      ; targets_seen = Pset.union targets t.targets_seen
      }
    in
    let on_error exn =
      With_required_by.reraise exn (Path requested_file)
    in
    Fiber.Var.set var t (Fiber.with_error_handler f ~on_error)
end

module Exec_status = struct
  type rule_evaluation = (Action.t * Pset.t) Fiber.Future.t
  type rule_execution = unit Fiber.Future.t

  type eval_rule = unit -> (Action.t * Pset.t) Fiber.t
  type exec_rule = rule_evaluation -> unit Fiber.t

  module Evaluating_rule = struct
    type t =
      { rule_evaluation : rule_evaluation
      ; exec_rule       : exec_rule
      }
  end
  module Running = struct
    type t =
      { rule_evaluation : rule_evaluation
      ; rule_execution  : rule_execution
      }
  end
  module Not_started = struct
    type t =
      { eval_rule : eval_rule
      ; exec_rule : exec_rule
      }
  end
  type t =
    | Not_started     of Not_started.t
    | Evaluating_rule of Evaluating_rule.t
    | Running         of Running.t
end

let rule_loc ~loc ~dir =
  match loc with
  | Some loc -> loc
  | None ->
    Loc.in_file
      (Path.to_string
         (Path.drop_optional_build_context (Path.relative dir "jbuild")))

module Internal_rule = struct
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> Ordering.t
    val gen : unit -> t
  end = struct
    type t = int
    let to_int x = x
    let compare (x : int) y = compare x y

    let counter = ref 0
    let gen () =
      let n = !counter in
      counter := n + 1;
      n
  end

  type t =
    { id               : Id.t
    ; rule_deps        : Pset.t
    ; static_deps      : Pset.t
    ; targets          : Pset.t
    ; context          : Context.t option
    ; build            : (unit, Action.t) Build.t
    ; mode             : Jbuild.Rule.Mode.t
    ; loc              : Loc.t option
    ; dir              : Path.t
    ; mutable exec     : Exec_status.t
    }

  let compare a b = Id.compare a.id b.id

  let loc ~dir t = rule_loc ~dir ~loc:t.loc
end

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file       : 'a Vfile_kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Some Eq
    | Sexp_file a    , Sexp_file b     -> Vfile_kind.eq a b
    | _                                -> None

  let eq_exn a b = Option.value_exn (eq a b)
end

module File_spec = struct
  type 'a t =
    { rule         : Internal_rule.t (* Rule which produces it *)
    ; mutable kind : 'a File_kind.t
    ; mutable data : 'a option
    }

  type packed = T : _ t -> packed

  let create rule kind =
    T { rule; kind; data = None }
end

module Alias0 = struct
  type t = { dir : Path.t; name : string }

  let pp fmt t = Path.pp fmt (Path.relative t.dir t.name)

  let suffix = "-" ^ String.make 32 '0'

  let of_path path =
    if not (Path.is_in_build_dir path) then
      die "Invalid alias!\nTried to reference alias %S"
        (Path.to_string_maybe_quoted path);
    { dir  = Path.parent path
    ; name = Path.basename path
    }

  let name t = t.name
  let dir  t = t.dir

  let fully_qualified_name t = Path.relative t.dir t.name

  let make name ~dir =
    assert (not (String.contains name '/'));
    { dir; name }

  let stamp_file t =
    Path.relative (Path.insert_after_build_dir_exn t.dir ".aliases") (t.name ^ suffix)

  let dep t = Build.path (stamp_file t)

  let is_standard = function
    | "runtest" | "install" | "doc" | "doc-private" | "lint" -> true
    | _ -> false

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    File_tree.Dir.fold dir ~traverse_ignored_dirs:false ~init:(Build.return true)
      ~f:(fun dir acc ->
        let path = Path.append ctx_dir (File_tree.Dir.path dir) in
        let fn = stamp_file (make ~dir:path name) in
        acc
        >>>
        Build.if_file_exists fn
          ~then_:(Build.path fn >>^ fun _ -> false)
          ~else_:(Build.arr (fun x -> x)))

  let dep_rec t ~loc ~file_tree =
    let ctx_dir, src_dir = Path.extract_build_context_dir t.dir |> Option.value_exn in
    match File_tree.find_dir file_tree src_dir with
    | None -> Build.fail { fail = fun () ->
      Loc.fail loc "Don't know about directory %s!" (Path.to_string_maybe_quoted src_dir) }
    | Some dir ->
      dep_rec_internal ~name:t.name ~dir ~ctx_dir
      >>^ fun is_empty ->
      if is_empty && not (is_standard t.name) then
        Loc.fail loc "This alias is empty.\n\
                      Alias %S is not defined in %s or any of its descendants."
          t.name (Path.to_string_maybe_quoted src_dir)

  let dep_rec_multi_contexts ~dir:src_dir ~name ~file_tree ~contexts =
    match File_tree.find_dir file_tree src_dir with
    | None ->
      die "From the command line:\n\
           @{<error>Error@}: Don't know about directory %s!" (Path.to_string_maybe_quoted src_dir)
    | Some dir ->
      let open Build.O in
      Build.all (List.map contexts ~f:(fun ctx ->
        let ctx_dir = Path.(relative build_dir) ctx in
        dep_rec_internal ~name ~dir ~ctx_dir))
      >>^ fun is_empty_list ->
      let is_empty = List.for_all is_empty_list ~f:(fun x -> x) in
      if is_empty && not (is_standard name) then
        die "From the command line:\n\
             @{<error>Error@}: Alias %s is empty.\n\
             It is not defined in %s or any of its descendants."
          name (Path.to_string_maybe_quoted src_dir)

  let default     = make "DEFAULT"
  let runtest     = make "runtest"
  let install     = make "install"
  let doc         = make "doc"
  let private_doc = make "doc-private"
  let lint        = make "lint"
end

module Dir_status = struct
  type waiting_for_load_dir =
    { mutable lazy_generators : (unit -> unit) list }

  type collection_stage =
    | Loading
    | Pending of waiting_for_load_dir

  type alias_action =
    { stamp  : Digest.t
    ; action : (unit, Action.t) Build.t
    ; locks  : Path.t list
    }


  type alias =
    { mutable deps    : Pset.t
    ; mutable actions : alias_action list
    }

  type rules_collector =
    { mutable rules   : Build_interpret.Rule.t list
    ; mutable aliases : alias String_map.t
    ; mutable stage   : collection_stage
    }

  type t =
    | Collecting_rules of rules_collector
    | Loaded  of Pset.t (* set of targets in the directory *)
    | Forward of Path.t (* Load this directory first       *)
    | Failed_to_load
end

module Files_of = struct
  type t =
    { files_by_ext   : Path.t list String_map.t
    ; dir_hash       : string
    ; mutable stamps : Path.t String_map.t
    }
end

type extra_sub_directories_to_keep =
  | All
  | These of String_set.t

type hook =
  | Rule_started
  | Rule_completed

type t =
  { (* File specification by targets *)
    files       : (Path.t, File_spec.packed) Hashtbl.t
  ; contexts    : Context.t String_map.t
  ; (* Table from target to digest of
       [(deps (filename + contents), targets (filename only), action)] *)
    trace       : (Path.t, Digest.t) Hashtbl.t
  ; file_tree   : File_tree.t
  ; mutable local_mkdirs : Path.Local.Set.t
  ; mutable dirs : (Path.t, Dir_status.t) Hashtbl.t
  ; mutable gen_rules : (dir:Path.t -> string list -> extra_sub_directories_to_keep) String_map.t
  ; mutable load_dir_stack : Path.t list
  ; (* Set of directories under _build that have at least one rule and
       all their ancestors. *)
    mutable build_dirs_to_keep : Path.Set.t
  ; files_of : (Path.t, Files_of.t) Hashtbl.t
  ; mutable prefix : (unit, unit) Build.t option
  ; hook : hook -> unit
  }

let string_of_paths set =
  Pset.to_list set
  |> List.map ~f:(fun p -> sprintf "- %s"
                             (Path.to_string_maybe_quoted
                                (Path.drop_optional_build_context p)))
  |> String.concat ~sep:"\n"

let set_rule_generators t generators =
  assert (String_map.keys generators = String_map.keys t.contexts);
  t.gen_rules <- generators

let get_dir_status t ~dir =
  Hashtbl.find_or_add t.dirs dir ~f:(fun _ ->
    if Path.is_in_source_tree dir then
      Dir_status.Loaded (File_tree.files_of t.file_tree dir)
    else if dir = Path.build_dir then
      (* Not allowed to look here *)
      Dir_status.Loaded Pset.empty
    else if not (Path.is_local dir) then
      Dir_status.Loaded
        (match Path.readdir dir with
         | exception _ -> Path.Set.empty
         | files ->
           Pset.of_list (List.map files ~f:(Path.relative dir)))
    else begin
      let (ctx, sub_dir) = Option.value_exn (Path.extract_build_context dir) in
      if ctx = ".aliases" then
        Forward (Path.(append build_dir) sub_dir)
      else if ctx <> "install" && not (String_map.mem t.contexts ctx) then
        Dir_status.Loaded Pset.empty
      else
        Collecting_rules
          { rules   = []
          ; aliases = String_map.empty
          ; stage   = Pending { lazy_generators = [] }
          }
    end)

let entry_point t ~f =
  (match t.load_dir_stack with
   | [] ->
     ()
   | _ :: _ ->
     code_errorf
       "Build_system.entry_point: called inside the rule generator callback");
  f ()

module Target = Build_interpret.Target
module Pre_rule = Build_interpret.Rule

let get_file : type a. t -> Path.t -> a File_kind.t -> a File_spec.t = fun t fn kind ->
  match Hashtbl.find t.files fn with
  | None -> die "no rule found for %s" (Path.to_string fn)
  | Some (File_spec.T file) ->
    let Eq = File_kind.eq_exn kind file.kind in
    file

let vfile_to_string (type a) (module K : Vfile_kind.S with type t = a) fn x =
  K.to_string fn x

module Build_exec = struct
  open Build.Repr

  let exec bs t x =
    let rec exec
      : type a b. Pset.t ref -> (a, b) t -> a -> b = fun dyn_deps t x ->
      match t with
      | Arr f -> f x
      | Targets _ -> x
      | Store_vfile (Vspec.T (fn, kind)) ->
        let file = get_file bs fn (Sexp_file kind) in
        file.data <- Some x;
        Write_file (fn, vfile_to_string kind fn x)
      | Compose (a, b) ->
        exec dyn_deps a x |> exec dyn_deps b
      | First t ->
        let x, y = x in
        (exec dyn_deps t x, y)
      | Second t ->
        let x, y = x in
        (x, exec dyn_deps t y)
      | Split (a, b) ->
        let x, y = x in
        let x = exec dyn_deps a x in
        let y = exec dyn_deps b y in
        (x, y)
      | Fanout (a, b) ->
        let a = exec dyn_deps a x in
        let b = exec dyn_deps b x in
        (a, b)
      | Paths _ -> x
      | Paths_glob state -> get_glob_result_exn state
      | Contents p -> Io.read_file (Path.to_string p)
      | Lines_of p -> Io.lines_of_file (Path.to_string p)
      | Vpath (Vspec.T (fn, kind)) ->
        let file : b File_spec.t = get_file bs fn (Sexp_file kind) in
        Option.value_exn file.data
      | Dyn_paths t ->
        let fns = exec dyn_deps t x in
        dyn_deps := Pset.union !dyn_deps (Pset.of_list fns);
        x
      | Record_lib_deps _ -> x
      | Fail { fail } -> fail ()
      | If_file_exists (_, state) ->
        exec dyn_deps (get_if_file_exists_exn state) x
      | Memo m ->
        match m.state with
        | Evaluated (x, deps) ->
          dyn_deps := Pset.union !dyn_deps deps;
          x
        | Evaluating ->
          die "Dependency cycle evaluating memoized build arrow %s" m.name
        | Unevaluated ->
          m.state <- Evaluating;
          let dyn_deps' = ref Pset.empty in
          match exec dyn_deps' m.t x with
          | x ->
            m.state <- Evaluated (x, !dyn_deps');
            dyn_deps := Pset.union !dyn_deps !dyn_deps';
            x
          | exception exn ->
            m.state <- Unevaluated;
            reraise exn
    in
    let dyn_deps = ref Pset.empty in
    let action = exec dyn_deps (Build.repr t) x in
    (action, !dyn_deps)

  let exec_nop bs t x =
    snd (exec bs (Build.O.(>>^) t (fun () -> Action.Progn [])) x)
end

(* [copy_source] is [true] for rules copying files from the source directory *)
let add_spec t fn spec ~copy_source =
  match Hashtbl.find t.files fn with
  | None ->
    Hashtbl.add t.files fn spec
  | Some (File_spec.T { rule; _ }) ->
    match copy_source, rule.mode with
    | true, (Standard | Not_a_rule_stanza) ->
      Loc.warn (Internal_rule.loc rule ~dir:(Path.parent fn))
        "File %s is both generated by a rule and present in the source tree.\n\
         As a result, the rule is currently ignored, however this will become an error \
         in the future.\n\
         %t"
        (maybe_quoted (Path.basename fn))
        (fun ppf ->
           match rule.mode with
           | Not_a_rule_stanza ->
             Format.fprintf ppf "Delete file %s to get rid of this warning."
               (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
           | Standard ->
             Format.fprintf ppf
               "To keep the current behavior and get rid of this warning, add a field \
                (fallback) to the rule."
           | _ -> assert false);
      Hashtbl.add t.files fn spec
    | _ ->
      let (File_spec.T { rule = rule2; _ }) = spec in
      let string_of_loc = function
        | None -> "<internal location>"
        | Some { Loc.start; _ } ->
          start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
      in
      die "Multiple rules generated for %s:\n\
           - %s\n\
           - %s"
        (Path.to_string_maybe_quoted fn)
        (if copy_source then
           "<internal copy rule>"
         else
           string_of_loc rule.loc)
        (string_of_loc rule2.loc)

let create_file_specs t targets rule ~copy_source =
  List.iter targets ~f:(function
    | Target.Normal fn ->
      add_spec t fn (File_spec.create rule Ignore_contents) ~copy_source
    | Target.Vfile (Vspec.T (fn, kind)) ->
      add_spec t fn (File_spec.create rule (Sexp_file kind)) ~copy_source)

(* This contains the targets of the actions that are being executed. On exit, we need to
   delete them as they might contain garbage *)
let pending_targets = ref Pset.empty

let () =
  at_exit (fun () ->
    let fns = !pending_targets in
    pending_targets := Pset.empty;
    Pset.iter fns ~f:Path.unlink_no_err)

let clear_targets_digests_after_rule_execution targets =
  let missing =
    List.fold_left targets ~init:Pset.empty ~f:(fun acc fn ->
      match Unix.lstat (Path.to_string fn) with
      | exception _ -> Pset.add acc fn
      | (_ : Unix.stats) ->
        Utils.Cached_digest.remove fn;
        acc)
  in
  if not (Pset.is_empty missing) then
    die "@{<error>Error@}: Rule failed to generate the following targets:\n%s"
      (string_of_paths missing)

let make_local_dirs t paths =
  Pset.iter paths ~f:(fun path ->
    match Path.kind path with
    | Local path ->
      if not (Path.Local.Set.mem t.local_mkdirs path) then begin
        Path.Local.mkdir_p path;
        t.local_mkdirs <- Path.Local.Set.add t.local_mkdirs path
      end
    | _ -> ())

let make_local_parent_dirs t paths ~map_path =
  Pset.iter paths ~f:(fun path ->
    match Path.kind (map_path path) with
    | Local path when not (Path.Local.is_root path) ->
      let parent = Path.Local.parent path in
      if not (Path.Local.Set.mem t.local_mkdirs parent) then begin
        Path.Local.mkdir_p parent;
        t.local_mkdirs <- Path.Local.Set.add t.local_mkdirs parent
      end
    | _ -> ())

let sandbox_dir = Path.of_string "_build/.sandbox"

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
     Hashtbl.mem t.files (Path.relative dir Config.jbuilder_keep_fname) then
    ()
  else
    match Path.readdir dir with
    | exception _ -> ()
    | files ->
      List.iter files ~f:(fun fn ->
        let path = Path.relative dir fn in
        match Unix.lstat (Path.to_string path) with
        | { st_kind = S_DIR; _ } -> begin
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if String_set.mem set fn ||
                 Pset.mem t.build_dirs_to_keep path then
                ()
              else
                Path.rm_rf path
          end
        | exception _ ->
          if not (Hashtbl.mem t.files path) then Path.unlink path
        | _ ->
          if not (Hashtbl.mem t.files path) then Path.unlink path)

let no_rule_found =
  let fail fn =
    die "No rule found for %s" (Utils.describe_target fn)
  in
  fun t fn ->
    match Path.extract_build_context fn with
    | None -> fail fn
    | Some (ctx, _) ->
      if String_map.mem t.contexts ctx then
        fail fn
      else
        die "Trying to build %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String_map.keys t.contexts))

let rec compile_rule t ?(copy_source=false) pre_rule =
  let { Pre_rule.
        context
      ; build
      ; targets = target_specs
      ; sandbox
      ; mode
      ; locks
      ; loc
      ; dir
      } =
    pre_rule
  in
  let targets = Target.paths target_specs in
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps = static_deps
      } = Build_interpret.static_deps build ~all_targets:(targets_of t)
            ~file_tree:t.file_tree
  in

  let eval_rule () =
    t.hook Rule_started;
    wait_for_deps t rule_deps
    >>| fun () ->
    Build_exec.exec t build ()
  in
  let exec_rule (rule_evaluation : Exec_status.rule_evaluation) =
    Fiber.fork_and_join_unit
      (fun () ->
         wait_for_deps t static_deps)
      (fun () ->
         Fiber.Future.wait rule_evaluation >>= fun (action, dyn_deps) ->
         wait_for_deps t (Pset.diff dyn_deps static_deps)
         >>| fun () ->
         (action, dyn_deps))
    >>= fun (action, dyn_deps) ->
    make_local_parent_dirs t targets ~map_path:(fun x -> x);
    let all_deps = Pset.union static_deps dyn_deps in
    let all_deps_as_list = Pset.to_list all_deps in
    let targets_as_list  = Pset.to_list targets  in
    let hash =
      let trace =
        (List.map all_deps_as_list ~f:(fun fn ->
           (fn, Utils.Cached_digest.file fn)),
         targets_as_list,
         Option.map context ~f:(fun c -> c.name),
         action)
      in
      Digest.string (Marshal.to_string trace [])
    in
    let sandbox_dir =
      if sandbox then
        Some (Path.relative sandbox_dir (Digest.to_hex hash))
      else
        None
    in
    let deps_or_rule_changed =
      List.fold_left targets_as_list ~init:false ~f:(fun acc fn ->
        match Hashtbl.find t.trace fn with
        | None ->
          Hashtbl.add t.trace fn hash;
          true
        | Some prev_hash ->
          Hashtbl.replace t.trace ~key:fn ~data:hash;
          acc || prev_hash <> hash)
    in
    let targets_missing =
      List.exists targets_as_list ~f:(fun fn ->
        match Unix.lstat (Path.to_string fn) with
        | exception _ -> true
        | (_ : Unix.stats) -> false)
    in
    let force =
      !Clflags.force &&
      List.exists targets_as_list ~f:Path.is_alias_stamp_file
    in
    if deps_or_rule_changed || targets_missing || force then begin
      List.iter targets_as_list ~f:Path.unlink_no_err;
      pending_targets := Pset.union targets !pending_targets;
      let action =
        match sandbox_dir with
        | Some sandbox_dir ->
          Path.rm_rf sandbox_dir;
          let sandboxed path =
            if Path.is_local path then
              Path.append sandbox_dir path
            else
              path
          in
          make_local_parent_dirs t all_deps ~map_path:sandboxed;
          make_local_parent_dirs t targets  ~map_path:sandboxed;
          Action.sandbox action
            ~sandboxed
            ~deps:all_deps_as_list
            ~targets:targets_as_list
        | None ->
          action
      in
      make_local_dirs t (Action.chdirs action);
      with_locks locks ~f:(fun () ->
        Action.exec ?context ~targets action) >>| fun () ->
      Option.iter sandbox_dir ~f:Path.rm_rf;
      (* All went well, these targets are no longer pending *)
      pending_targets := Pset.diff !pending_targets targets;
      clear_targets_digests_after_rule_execution targets_as_list;
      (match mode with
       | Standard | Fallback | Not_a_rule_stanza | Ignore_source_files -> ()
       | Promote | Promote_but_delete_on_clean ->
         Pset.iter targets ~f:(fun path ->
           let in_source_tree = Option.value_exn (Path.drop_build_context path) in
           if mode = Promote_but_delete_on_clean then
             Promoted_to_delete.add in_source_tree;
           Io.copy_file
             ~src:(Path.to_string path)
             ~dst:(Path.to_string in_source_tree)));
      t.hook Rule_completed
    end else begin
      t.hook Rule_completed;
      Fiber.return ()
    end
  in
  let rule =
    { Internal_rule.
      id = Internal_rule.Id.gen ()
    ; static_deps
    ; rule_deps
    ; targets
    ; build
    ; context
    ; exec = Not_started { eval_rule; exec_rule }
    ; mode
    ; loc
    ; dir
    }
  in
  create_file_specs t target_specs rule ~copy_source

and setup_copy_rules t ~ctx_dir ~non_target_source_files =
  Pset.iter non_target_source_files ~f:(fun path ->
    let ctx_path = Path.append ctx_dir path in
    let build = Build.copy ~src:path ~dst:ctx_path in
    (* We temporarily allow overrides while setting up copy rules from
       the source directory so that artifact that are already present
       in the source directory are not re-computed.

       This allows to keep generated files in tarballs. Maybe we
       should allow it on a case-by-case basis though. *)
    compile_rule t (Pre_rule.make build) ~copy_source:true)

and load_dir   t ~dir = ignore (load_dir_and_get_targets t ~dir : Pset.t)
and targets_of t ~dir =         load_dir_and_get_targets t ~dir

and load_dir_and_get_targets t ~dir =
  match get_dir_status t ~dir with
  | Failed_to_load -> raise Already_reported

  | Loaded targets -> targets

  | Forward dir' ->
    load_dir t ~dir:dir';
    begin match get_dir_status t ~dir with
    | Loaded targets -> targets
    | _ -> assert false
    end

  | Collecting_rules collector ->
    let lazy_generators =
      match collector.stage with
      | Loading ->
        die "recursive dependency between directories:\n    %s"
          (String.concat ~sep:"\n--> "
             (List.map t.load_dir_stack ~f:Utils.describe_target))
      | Pending { lazy_generators } ->
        collector.stage <- Loading;
        lazy_generators
    in

    collector.stage <- Loading;
    t.load_dir_stack <- dir :: t.load_dir_stack;

    try
      load_dir_step2_exn t ~dir ~collector ~lazy_generators
    with exn ->
      (match Hashtbl.find t.dirs dir with
       | Some (Loaded _) -> ()
       | _ ->
         (match t.load_dir_stack with
          | [] -> assert false
          | x :: l ->
            t.load_dir_stack <- l;
            assert (x = dir)));
      Hashtbl.replace t.dirs ~key:dir ~data:Failed_to_load;
      reraise exn

and load_dir_step2_exn t ~dir ~collector ~lazy_generators =
  List.iter lazy_generators ~f:(fun f -> f ());

  let context_name, sub_dir = Option.value_exn (Path.extract_build_context dir) in

  (* Load all the rules *)
  let extra_subdirs_to_keep =
    if context_name = "install" then
      These String_set.empty
    else
      let gen_rules = Option.value_exn (String_map.find t.gen_rules context_name) in
      gen_rules ~dir (Option.value_exn (Path.explode sub_dir))
  in
  let rules = collector.rules in

  (* Compute alias rules *)
  let alias_dir = Path.append (Path.relative alias_dir context_name) sub_dir in
  let alias_rules, alias_stamp_files =
    let open Build.O in
    String_map.foldi collector.aliases ~init:([], Pset.empty)
      ~f:(fun name { Dir_status. deps; actions } (rules, alias_stamp_files) ->
        let base_path = Path.relative alias_dir name in
        let rules, deps =
          List.fold_left actions ~init:(rules, deps)
            ~f:(fun (rules, deps) { Dir_status. stamp; action; locks } ->
              let path =
                Path.extend_basename base_path
                  ~suffix:("-" ^ Digest.to_hex stamp)
              in
              let rule =
                Pre_rule.make ~locks
                  (Build.progn [ action; Build.create_file path ])
              in
              (rule :: rules, Pset.add deps path))
        in
        let path = Path.extend_basename base_path ~suffix:Alias0.suffix in
        (Pre_rule.make
           (Build.path_set deps >>>
            Build.action ~targets:[path]
              (Redirect (Stdout,
                         path,
                         Digest_files
                           (Path.Set.to_list deps))))
         :: rules,
         Pset.add alias_stamp_files path))
  in
  Hashtbl.replace t.dirs ~key:alias_dir ~data:(Loaded alias_stamp_files);

  (* Compute the set of targets and the set of source files that must not be copied *)
  let user_rule_targets, source_files_to_ignore =
    List.fold_left rules ~init:(Pset.empty, Pset.empty)
      ~f:(fun (acc_targets, acc_ignored) { Pre_rule.targets; mode; _ } ->
        let targets = Build_interpret.Target.paths targets in
        (Pset.union targets acc_targets,
         match mode with
         | Promote | Promote_but_delete_on_clean | Ignore_source_files ->
           Pset.union targets acc_ignored
         | _ ->
           acc_ignored))
  in
  let source_files_to_ignore =
    Pset.map source_files_to_ignore ~f:(fun p ->
      Option.value_exn (Path.drop_build_context p))
  in

  (* Take into account the source files *)
  let targets, to_copy, subdirs_to_keep =
    match context_name with
    | "install" ->
      (user_rule_targets,
       None,
       String_set.empty)
    | ctx_name ->
      (* This condition is [true] because of [get_dir_status] *)
      assert (String_map.mem t.contexts ctx_name);
      let files, subdirs =
        match File_tree.find_dir t.file_tree sub_dir with
        | None -> (Pset.empty, String_set.empty)
        | Some dir ->
          (File_tree.Dir.file_paths    dir,
           File_tree.Dir.sub_dir_names dir)
      in
      let files = Pset.diff files source_files_to_ignore in
      if Pset.is_empty files then
        (user_rule_targets, None, subdirs)
      else
        let ctx_path = Path.(relative build_dir) context_name in
        (Pset.union user_rule_targets
           (Pset.map files ~f:(Path.append ctx_path)),
         Some (ctx_path, files),
         subdirs)
  in
  let subdirs_to_keep =
    match extra_subdirs_to_keep with
    | All -> All
    | These set -> These (String_set.union subdirs_to_keep set)
  in

  (* Filter out fallback rules *)
  let rules =
    match to_copy with
    | None ->
      (* If there are no source files to copy, fallback rules are
         automatically kept *)
      rules
    | Some (_, to_copy) ->
      List.filter rules ~f:(fun (rule : Build_interpret.Rule.t) ->
        match rule.mode with
        | Standard | Promote | Promote_but_delete_on_clean
        | Not_a_rule_stanza | Ignore_source_files -> true
        | Fallback ->
          let source_files_for_targtes =
            List.fold_left rule.targets ~init:Pset.empty
              ~f:(fun acc target ->
                Pset.add acc
                  (Build_interpret.Target.path target
                   |> Path.drop_build_context
                   (* All targets are in [dir] and we know it
                      correspond to a directory of a build context
                      since there are source files to copy, so this
                      call can't fail. *)
                   |> Option.value_exn))
          in
          if Pset.is_subset source_files_for_targtes ~of_:to_copy then
            (* All targets are present *)
            false
          else begin
            if Pset.is_empty (Pset.inter source_files_for_targtes to_copy) then
              (* No target is present *)
              true
            else begin
              let absent_targets =
                Pset.diff source_files_for_targtes to_copy
              in
              let present_targets =
                Pset.diff source_files_for_targtes absent_targets
              in
              Loc.fail
                (rule_loc ~loc:rule.loc
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
                (string_of_paths present_targets)
                (string_of_paths absent_targets)
            end
          end)
  in

  (* Set the directory status to loaded *)
  Hashtbl.replace t.dirs ~key:dir ~data:(Loaded targets);
  (match t.load_dir_stack with
   | [] -> assert false
   | x :: l ->
     t.load_dir_stack <- l;
     assert (x = dir));

  (* Compile the rules and cleanup stale artifacts *)
  List.iter rules ~f:(compile_rule t ~copy_source:false);
  Option.iter to_copy ~f:(fun (ctx_dir, source_files) ->
    setup_copy_rules t ~ctx_dir ~non_target_source_files:source_files);
  remove_old_artifacts t ~dir ~subdirs_to_keep;

  List.iter alias_rules ~f:(compile_rule t ~copy_source:false);
  remove_old_artifacts t ~dir:alias_dir ~subdirs_to_keep;

  targets

and wait_for_file t fn =
  match Hashtbl.find t.files fn with
  | Some file -> wait_for_file_found fn file
  | None ->
    let dir = Path.parent fn in
    if Path.is_in_build_dir dir then begin
      load_dir t ~dir;
      match Hashtbl.find t.files fn with
      | Some file -> wait_for_file_found fn file
      | None -> no_rule_found t fn
    end else if Path.exists fn then
      Fiber.return ()
    else
      die "File unavailable: %s" (Path.to_string_maybe_quoted fn)

and wait_for_file_found fn (File_spec.T file) =
  Dependency_path.push fn ~targets:file.rule.targets ~f:(fun () ->
    match file.rule.exec with
    | Not_started { eval_rule; exec_rule } ->
      Fiber.fork eval_rule
      >>= fun rule_evaluation ->
      Fiber.fork (fun () -> exec_rule rule_evaluation)
      >>= fun rule_execution ->
      file.rule.exec <-
        Running { rule_evaluation
                ; rule_execution
                };
      Fiber.Future.wait rule_execution
    | Running { rule_execution; _ } ->
      Fiber.Future.wait rule_execution
    | Evaluating_rule { rule_evaluation; exec_rule } ->
      Fiber.fork (fun () -> exec_rule rule_evaluation)
      >>= fun rule_execution ->
      file.rule.exec <-
        Running { rule_evaluation
                ; rule_execution
                };
      Fiber.Future.wait rule_execution)

and wait_for_deps t deps =
  Fiber.parallel_iter (Pset.to_list deps) ~f:(wait_for_file t)

let stamp_file_for_files_of t ~dir ~ext =
  let files_of_dir =
    Hashtbl.find_or_add t.files_of dir ~f:(fun dir ->
      let files_by_ext =
        targets_of t ~dir
        |> Path.Set.to_list
        |> List.map ~f:(fun fn -> Filename.extension (Path.to_string fn), fn)
        |> String_map.of_list_multi
      in
      { files_by_ext
      ; dir_hash = Path.to_string dir |> Digest.string |> Digest.to_hex
      ; stamps = String_map.empty
      })
  in
  match String_map.find files_of_dir.stamps ext with
  | Some fn -> fn
  | None ->
    let stamp_file = Path.relative misc_dir (files_of_dir.dir_hash ^ ext) in
    let files =
      Option.value
        (String_map.find files_of_dir.files_by_ext ext)
        ~default:[]
    in
    compile_rule t
      (let open Build.O in
       Pre_rule.make
         (Build.paths files >>>
          Build.action ~targets:[stamp_file]
            (Action.with_stdout_to stamp_file
               (Action.digest_files files))));
    files_of_dir.stamps <- String_map.add files_of_dir.stamps ext stamp_file;
    stamp_file

module Trace = struct
  type t = (Path.t, Digest.t) Hashtbl.t

  let file = "_build/.db"

  let dump (trace : t) =
    let sexp =
      Sexp.List (
        Hashtbl.foldi trace ~init:Pmap.empty ~f:(fun key data acc ->
          Pmap.add acc key data)
        |> Path.Map.to_list
        |> List.map ~f:(fun (path, hash) ->
               Sexp.List [ Path.sexp_of_t path;
                           Atom (Sexp.Atom.of_digest hash) ]))
    in
    if Sys.file_exists "_build" then
      Io.write_file file (Sexp.to_string sexp)

  let load () =
    let trace = Hashtbl.create 1024 in
    if Sys.file_exists file then begin
      let sexp = Sexp.load ~fname:file ~mode:Single in
      let bindings =
        let open Sexp.Of_sexp in
        list (pair Path.t (fun s -> Digest.from_hex (string s))) sexp
      in
      List.iter bindings ~f:(fun (path, hash) ->
        Hashtbl.add trace path hash);
    end;
    trace
end

let all_targets t =
  String_map.iter t.contexts ~f:(fun ctx ->
    File_tree.fold t.file_tree ~traverse_ignored_dirs:true ~init:()
      ~f:(fun dir () ->
        load_dir t
          ~dir:(Path.append ctx.Context.build_dir (File_tree.Dir.path dir))));
  Hashtbl.foldi t.files ~init:[] ~f:(fun key _ acc -> key :: acc)

let finalize t =
  (* Promotion must be handled before dumping the digest cache, as it
     might delete some entries. *)
  Action.Promotion.finalize ();
  Promoted_to_delete.dump ();
  Utils.Cached_digest.dump ();
  Trace.dump t.trace

let create ~contexts ~file_tree ~hook =
  Utils.Cached_digest.load ();
  let contexts =
    List.map contexts ~f:(fun c -> (c.Context.name, c))
    |> String_map.of_list_exn
  in
  let t =
    { contexts
    ; files      = Hashtbl.create 1024
    ; trace      = Trace.load ()
    ; local_mkdirs = Path.Local.Set.empty
    ; dirs       = Hashtbl.create 1024
    ; load_dir_stack = []
    ; file_tree
    ; gen_rules = String_map.map contexts ~f:(fun _ ~dir:_ ->
        die "gen_rules called too early")
    ; build_dirs_to_keep = Pset.empty
    ; files_of = Hashtbl.create 1024
    ; prefix = None
    ; hook
    }
  in
  at_exit (fun () -> finalize t);
  t

let eval_request t ~request ~process_target =
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps = static_deps
      } = Build_interpret.static_deps request ~all_targets:(targets_of t)
            ~file_tree:t.file_tree
  in

  let process_targets ts =
    Fiber.parallel_iter (Pset.to_list ts) ~f:process_target
  in

  Fiber.fork_and_join_unit
    (fun () -> process_targets static_deps)
    (fun () ->
       wait_for_deps t rule_deps
       >>= fun () ->
       let dyn_deps = Build_exec.exec_nop t request () in
       process_targets (Pset.diff dyn_deps static_deps))

let do_build t ~request =
  entry_point t ~f:(fun () ->
    eval_request t ~request ~process_target:(wait_for_file t))

module Ir_set = Set.Make(Internal_rule)

let rules_for_files t paths =
  List.filter_map paths ~f:(fun path ->
    if Path.is_in_build_dir path then load_dir t ~dir:(Path.parent path);
    match Hashtbl.find t.files path with
    | None -> None
    | Some (File_spec.T { rule; _ }) -> Some rule)
  |> Ir_set.of_list
  |> Ir_set.to_list

module Ir_closure =
  Top_closure.Make(Internal_rule.Id)
    (struct
      type graph = t
      type t = Internal_rule.t
      let key (t : t) = t.id
      let deps (t : t) bs =
        rules_for_files bs
          (Pset.to_list
             (Pset.union
                t.static_deps
                t.rule_deps))
    end)

let rules_for_targets t targets =
  match Ir_closure.top_closure t (rules_for_files t targets) with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun rule ->
         Path.to_string (Option.value_exn
                           (Pset.choose rule.Internal_rule.targets)))
       |> String.concat ~sep:"\n-> ")

let static_deps_of_request t request =
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps
      } = Build_interpret.static_deps request ~all_targets:(targets_of t)
            ~file_tree:t.file_tree
  in
  Pset.to_list (Pset.union rule_deps action_deps)

let all_lib_deps t ~request =
  let targets = static_deps_of_request t request in
  List.fold_left (rules_for_targets t targets) ~init:Pmap.empty
    ~f:(fun acc (rule : Internal_rule.t) ->
      let deps = Build_interpret.lib_deps rule.build in
      if String_map.is_empty deps then
        acc
      else
        let deps =
          match Pmap.find acc rule.dir with
          | None -> deps
          | Some deps' -> Build.merge_lib_deps deps deps'
        in
        Pmap.add acc rule.dir deps)

let all_lib_deps_by_context t ~request =
  let targets = static_deps_of_request t request in
  let rules = rules_for_targets t targets in
  List.fold_left rules ~init:[] ~f:(fun acc (rule : Internal_rule.t) ->
    let deps = Build_interpret.lib_deps rule.build in
    if String_map.is_empty deps then
      acc
    else
      match Path.extract_build_context rule.dir with
      | None -> acc
      | Some (context, _) -> (context, deps) :: acc)
  |> String_map.of_list_multi
  |> String_map.map ~f:(function
    | [] -> String_map.empty
    | x :: l -> List.fold_left l ~init:x ~f:Build.merge_lib_deps)

module Rule = struct
  module Id = Internal_rule.Id

  type t =
    { id      : Id.t
    ; deps    : Path.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }

  let compare a b = Id.compare a.id b.id
end

module Rule_set = Set.Make(Rule)
module Id_set = Set.Make(Rule.Id)

let rules_for_files rules paths =
  List.fold_left paths ~init:Rule_set.empty ~f:(fun acc path ->
    match Pmap.find rules path with
    | None -> acc
    | Some rule -> Rule_set.add acc rule)
  |> Rule_set.to_list

module Rule_closure =
  Top_closure.Make(Rule.Id)
    (struct
      type graph = Rule.t Pmap.t
      type t = Rule.t
      let key (t : t) = t.id
      let deps (t : t) (graph : graph) =
        rules_for_files graph (Pset.to_list t.deps)
    end)

let build_rules_internal ?(recursive=false) t ~request =
  let rules_seen = ref Id_set.empty in
  let rules = ref [] in
  let rec loop fn =
    let dir = Path.parent fn in
    if Path.is_in_build_dir dir then load_dir t ~dir;
    match Hashtbl.find t.files fn with
    | Some file ->
      file_found fn file
    | None ->
      Fiber.return ()
  and file_found fn (File_spec.T { rule = ir; _ }) =
    if Id_set.mem !rules_seen ir.id then
      Fiber.return ()
    else begin
      rules_seen := Id_set.add !rules_seen ir.id;
      (match ir.exec with
       | Running { rule_evaluation; _ } | Evaluating_rule { rule_evaluation; _ } ->
         Fiber.return rule_evaluation
       | Not_started { eval_rule; exec_rule } ->
         Fiber.fork (fun () ->
           Dependency_path.push fn ~targets:ir.targets ~f:eval_rule)
         >>| fun rule_evaluation ->
         ir.exec <- Evaluating_rule { rule_evaluation
                                    ; exec_rule
                                    };
         rule_evaluation)
      >>= fun rule_evaluation ->
      Fiber.fork (fun () ->
        Fiber.Future.wait rule_evaluation
        >>| fun (action, dyn_deps) ->
        { Rule.
          id      = ir.id
        ; deps    = Pset.union ir.static_deps dyn_deps
        ; targets = ir.targets
        ; context = ir.context
        ; action  = action
        })
      >>= fun rule ->
      rules := rule :: !rules;
      if not recursive then
        Fiber.return ()
      else
        Fiber.Future.wait rule >>= fun rule ->
        Fiber.parallel_iter (Pset.to_list rule.deps) ~f:loop
    end
  in
  let targets = ref Pset.empty in
  eval_request t ~request ~process_target:(fun fn ->
    targets := Pset.add !targets fn;
    loop fn)
  >>= fun () ->
  Fiber.all (List.map !rules ~f:Fiber.Future.wait)
  >>| fun rules ->
  let rules =
    List.fold_left rules ~init:Pmap.empty ~f:(fun acc (r : Rule.t) ->
      Pset.fold r.targets ~init:acc ~f:(fun fn acc ->
        Pmap.add acc fn r))
  in
  match
    Rule_closure.top_closure rules
      (rules_for_files rules (Pset.to_list !targets))
  with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun rule ->
         Path.to_string (Option.value_exn (Pset.choose rule.Rule.targets)))
       |> String.concat ~sep:"\n-> ")

let build_rules ?recursive t ~request =
  entry_point t ~f:(fun () ->
    build_rules_internal ?recursive t ~request)

(* +-----------------------------------------------------------------+
   | Adding rules to the system                                      |
   +-----------------------------------------------------------------+ *)

let rec add_build_dir_to_keep t ~dir =
  if not (Pset.mem t.build_dirs_to_keep dir) then begin
    t.build_dirs_to_keep <- Pset.add t.build_dirs_to_keep dir;
    let dir = Path.parent dir in
    if dir <> Path.root then
      add_build_dir_to_keep t ~dir
  end

let get_collector t ~dir =
  match get_dir_status t ~dir with
  | Collecting_rules collector ->
    if collector.rules = [] && String_map.is_empty collector.aliases then
      add_build_dir_to_keep t ~dir;
    collector
  | Failed_to_load -> raise Already_reported
  | Loaded _ | Forward _ ->
    Sexp.code_error
      (if Path.is_in_source_tree dir then
         "Build_system.get_collector called on source directory"
       else if dir = Path.build_dir then
         "Build_system.get_collector called on _build"
       else if not (Path.is_local dir) then
         "Build_system.get_collector called on external directory"
       else
         "Build_system.get_collector called on closed directory")
      [ "dir", Path.sexp_of_t dir
      ]

let add_rule t (rule : Build_interpret.Rule.t) =
  let rule =
    match t.prefix with
    | None -> rule
    | Some prefix -> { rule with build = Build.O.(>>>) prefix rule.build } in
  let collector = get_collector t ~dir:rule.dir in
  collector.rules <- rule :: collector.rules

let prefix_rules' t prefix ~f =
  let old_prefix = t.prefix in
  t.prefix <- prefix;
  protectx () ~f ~finally:(fun () -> t.prefix <- old_prefix)

let prefix_rules t prefix ~f =
  begin match Build_interpret.targets prefix with
  | [] -> ()
  | targets ->
    Sexp.code_error "Build_system.prefix_rules' prefix contains targets"
      ["targets", Path.Set.sexp_of_t (Build_interpret.Target.paths targets)]
  end;
  let prefix =
    match t.prefix with
    | None -> prefix
    | Some p -> Build.O.(>>>) p prefix
  in
  prefix_rules' t (Some prefix) ~f

let on_load_dir t ~dir ~f =
  let collector = get_collector t ~dir in
  let current_prefix = t.prefix in
  let f () = prefix_rules' t current_prefix ~f in
  match collector.stage with
  | Loading -> f ()
  | Pending p ->
    let lazy_generators = p.lazy_generators in
    if lazy_generators = [] &&
       collector.rules = [] &&
       String_map.is_empty collector.aliases then
      add_build_dir_to_keep t ~dir;
    p.lazy_generators <- f :: lazy_generators

let eval_glob t ~dir re =
  let targets = targets_of t ~dir |> Pset.to_list |> List.map ~f:Path.basename in
  let files =
    match File_tree.find_dir t.file_tree dir with
    | None -> targets
    | Some d ->
      String_set.union (String_set.of_list targets) (File_tree.Dir.files d)
      |> String_set.to_list
  in
  List.filter files ~f:(Re.execp re)

module Alias = struct
  include Alias0

  let get_alias_def build_system t =
    let collector = get_collector build_system ~dir:t.dir in
    match String_map.find collector.aliases t.name with
    | None ->
      let x = { Dir_status. deps = Pset.empty; actions = [] } in
      collector.aliases <- String_map.add collector.aliases t.name x;
      x
    | Some x -> x

  let add_deps build_system t deps =
    let def = get_alias_def build_system t in
    def.deps <- Pset.union def.deps (Pset.of_list deps)

  let add_action build_system t ?(locks=[]) ~stamp action =
    let def = get_alias_def build_system t in
    def.actions <- { stamp = Digest.string (Sexp.to_string stamp)
                   ; action
                   ; locks
                   } :: def.actions
end

let is_target t file = Pset.mem (targets_of t ~dir:(Path.parent file)) file
