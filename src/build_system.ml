open! Stdune
open Import
open Fiber.O

module Vspec = Build.Vspec

(* Where we store stamp files for aliases *)
let alias_dir = Path.(relative build_dir) ".aliases"

(* Where we store stamp files for [stamp_file_for_files_of] *)
let misc_dir = Path.(relative build_dir) ".misc"

let () = Hooks.End_of_build.always Memo.reset

module Promoted_to_delete = struct
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

let rule_loc ~file_tree ~loc ~dir =
  match loc with
  | Some loc -> loc
  | None ->
    let dir = Path.drop_optional_build_context dir in
    let file =
      match
        Option.bind (File_tree.find_dir file_tree dir)
          ~f:File_tree.Dir.dune_file
      with
      | Some file -> File_tree.Dune_file.path file
      | None      -> Path.relative dir "_unknown_"
    in
    Loc.in_file (Path.to_string file)

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

  type t =
    { id               : Id.t
    ; static_deps      : Static_deps.t Fiber.Once.t
    ; targets          : Path.Set.t
    ; context          : Context.t option
    ; build            : (unit, Action.t) Build.t
    ; mode             : Dune_file.Rule.Mode.t
    ; loc              : Loc.t option
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
  let equal a b = Id.equal a.id b.id
  let hash t = Id.hash t.id

  let loc ~file_tree ~dir t  = rule_loc ~file_tree ~dir ~loc:t.loc

  let to_sexp t : Sexp.t =
    Sexp.Encoder.record
      [ "id", Id.to_sexp t.id
      ; "loc", Sexp.Encoder.option Loc.to_sexp t.loc
      ]

  let lib_deps t =
    (* Forcing this lazy ensures that the various globs and
       [if_file_exists] are resolved inside the [Build.t] value. *)
    Fiber.Once.get t.static_deps
    >>| (fun _ -> Build_interpret.lib_deps t.build)

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
    ; loc         = None
    ; dir         = Path.root
    ; env         = None
    ; sandbox     = false
    ; locks       = []
    ; rev_deps    = []
    ; transitive_rev_deps = Id.Set.empty
    }

  let make_request ~build ~static_deps =
    { root with
      id = Id.gen ()
    ; static_deps
    ; build
    }
end

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file       : 'a Vfile_kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) Type_eq.t option = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Some Type_eq.T
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
  module T : sig
    type t = private
      { dir : Path.t
      ; name : string
      }
    val make : string -> dir:Path.t -> t
    val of_user_written_path : loc:Loc.t -> Path.t -> t
  end = struct
    type t =
      { dir : Path.t
      ; name : string
      }

    let make name ~dir =
      if not (Path.is_in_build_dir dir) || String.contains name '/' then
        Exn.code_error "Alias0.make: Invalid alias"
          [ "name", Sexp.Encoder.string name
          ; "dir", Path.to_sexp dir
          ];
      { dir; name }

    let of_user_written_path ~loc path =
      if not (Path.is_in_build_dir path) then
        Errors.fail loc "Invalid alias!\n\
                      Tried to reference path outside build dir: %S"
          (Path.to_string_maybe_quoted path);
      { dir = Path.parent_exn path
      ; name = Path.basename path
      }
  end
  include T

  let pp fmt t = Path.pp fmt (Path.relative t.dir t.name)

  let suffix = "-" ^ String.make 32 '0'

  let name t = t.name
  let dir  t = t.dir

  let fully_qualified_name t = Path.relative t.dir t.name

  let stamp_file t =
    Path.relative (Path.insert_after_build_dir_exn t.dir ".aliases")
      (t.name ^ suffix)

  let dep t = Build.path (stamp_file t)

  let find_dir_specified_on_command_line ~dir ~file_tree =
    match File_tree.find_dir file_tree dir with
    | None ->
      die "From the command line:\n\
           @{<error>Error@}: Don't know about directory %s!"
        (Path.to_string_maybe_quoted dir)
    | Some dir -> dir

  let dep_multi_contexts ~dir ~name ~file_tree ~contexts =
    ignore
      (find_dir_specified_on_command_line ~dir ~file_tree : File_tree.Dir.t);
    Build.paths (List.map contexts ~f:(fun ctx ->
      let dir = Path.append (Path.(relative build_dir) ctx) dir in
      stamp_file (make ~dir name)))

  let standard_aliases = Hashtbl.create 7

  let is_standard = Hashtbl.mem standard_aliases

  let make_standard name =
    Hashtbl.add standard_aliases name ();
    make name

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    Build.lazy_no_targets (lazy (
      File_tree.Dir.fold dir ~traverse_ignored_dirs:false
        ~init:(Build.return true)
        ~f:(fun dir acc ->
          let path = Path.append ctx_dir (File_tree.Dir.path dir) in
          let fn = stamp_file (make ~dir:path name) in
          acc
          >>>
          Build.if_file_exists fn
            ~then_:(Build.path fn >>^ fun _ -> false)
            ~else_:(Build.arr (fun x -> x)))))

  let dep_rec t ~loc ~file_tree =
    let ctx_dir, src_dir =
      Path.extract_build_context_dir t.dir |> Option.value_exn
    in
    match File_tree.find_dir file_tree src_dir with
    | None ->
      Build.fail { fail = fun () ->
        Errors.fail loc "Don't know about directory %s!"
          (Path.to_string_maybe_quoted src_dir) }
    | Some dir ->
      dep_rec_internal ~name:t.name ~dir ~ctx_dir
      >>^ fun is_empty ->
      if is_empty && not (is_standard t.name) then
        Errors.fail loc
          "This alias is empty.\n\
           Alias %S is not defined in %s or any of its descendants."
          t.name (Path.to_string_maybe_quoted src_dir)

  let dep_rec_multi_contexts ~dir:src_dir ~name ~file_tree ~contexts =
    let open Build.O in
    let dir = find_dir_specified_on_command_line ~dir:src_dir ~file_tree in
    Build.all (List.map contexts ~f:(fun ctx ->
      let ctx_dir = Path.(relative build_dir) ctx in
      dep_rec_internal ~name ~dir ~ctx_dir))
    >>^ fun is_empty_list ->
    let is_empty = List.for_all is_empty_list ~f:(fun x -> x) in
    if is_empty && not (is_standard name) then
      die "From the command line:\n\
           @{<error>Error@}: Alias %S is empty.\n\
           It is not defined in %s or any of its descendants."
        name (Path.to_string_maybe_quoted src_dir)

  let default     = make_standard "default"
  let runtest     = make_standard "runtest"
  let install     = make_standard "install"
  let doc         = make_standard "doc"
  let private_doc = make_standard "doc-private"
  let lint        = make_standard "lint"
  let all         = make_standard "all"
  let check       = make_standard "check"
  let fmt         = make_standard "fmt"

  let package_install ~(context : Context.t) ~pkg =
    make (sprintf ".%s-files" (Package.Name.to_string pkg))
      ~dir:context.build_dir
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
    ; context : Context.t
    ; env : Env.t option
    ; loc : Loc.t option
    }


  type alias =
    { mutable deps     : Path.Set.t
    ; mutable dyn_deps : (unit, Path.Set.t) Build.t
    ; mutable actions  : alias_action list
    }

  type rules_collector =
    { mutable rules   : Build_interpret.Rule.t list
    ; mutable aliases : alias String.Map.t
    ; mutable stage   : collection_stage
    }

  type t =
    | Collecting_rules of rules_collector
    | Loaded  of Path.Set.t (* set of targets in the directory *)
    | Forward of Path.t (* Load this directory first       *)
    | Failed_to_load
end

module Files_of = struct
  type t =
    { files_by_ext   : Path.t list String.Map.t
    ; dir_hash       : string
    ; mutable stamps : Path.t String.Map.t
    }
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
  type t = Action.t * Deps.t
  let equal = (=)
  let hash = Hashtbl.hash
  let to_sexp (action, deps) =
    Sexp.Encoder.record
      [ "action", Dune_lang.to_sexp
                    (Action.For_shell.encode (Action.for_shell action))
       ; "deps", Dune_lang.to_sexp (Deps.to_sexp deps)
      ]
end

module Rule_fn = Memo.Make_hidden(Internal_rule)
module Path_fn = Memo.Make(Path)(Path_dune_lang)

type t =
  { (* File specification by targets *)
    files       : File_spec.packed Path.Table.t
  ; contexts    : Context.t String.Map.t
  ; file_tree   : File_tree.t
  ; mutable local_mkdirs : Path.Set.t
  ; mutable dirs : Dir_status.t Path.Table.t
  ; mutable gen_rules :
      (dir:Path.t -> string list -> extra_sub_directories_to_keep) String.Map.t
  ; mutable load_dir_stack : Path.t list
  ; (* Set of directories under _build that have at least one rule and
       all their ancestors. *)
    mutable build_dirs_to_keep : Path.Set.t
  ; files_of : Files_of.t Path.Table.t
  ; mutable prefix : (unit, unit) Build.t option
  ; hook : hook -> unit
  ; (* Package files are part of *)
    packages : Package.Name.t Path.Table.t
  (* memoized functions *)
  ; prepare_rule_def : (Internal_rule.t -> Action_and_deps.t Fiber.t) Fdecl.t
  ; build_rule_def : Action_and_deps.t Rule_fn.t Fdecl.t
  ; build_file_def : unit Path_fn.t Fdecl.t
  ; build_rule_internal_def : (Internal_rule.t -> unit Fiber.t) Fdecl.t
  }

let string_of_paths set =
  Path.Set.to_list set
  |> List.map ~f:(fun p -> sprintf "- %s"
                             (Path.to_string_maybe_quoted
                                (Path.drop_optional_build_context p)))
  |> String.concat ~sep:"\n"

let set_rule_generators t generators =
  assert (String.Map.keys generators = String.Map.keys t.contexts);
  t.gen_rules <- generators

let get_dir_status t ~dir =
  Path.Table.find_or_add t.dirs dir ~f:(fun _ ->
    if Path.is_in_source_tree dir then
      Dir_status.Loaded (File_tree.files_of t.file_tree dir)
    else if Path.equal dir Path.build_dir then
      (* Not allowed to look here *)
      Dir_status.Loaded Path.Set.empty
    else if not (Path.is_managed dir) then
      Dir_status.Loaded
        (match Path.readdir_unsorted dir with
         | exception _ -> Path.Set.empty
         | files ->
           Path.Set.of_list (List.map files ~f:(Path.relative dir)))
    else begin
      let (ctx, sub_dir) = Option.value_exn (Path.extract_build_context dir) in
      if ctx = ".aliases" then
        Forward (Path.(append build_dir) sub_dir)
      else if ctx <> "install" && not (String.Map.mem t.contexts ctx) then
        Dir_status.Loaded Path.Set.empty
      else
        Collecting_rules
          { rules   = []
          ; aliases = String.Map.empty
          ; stage   = Pending { lazy_generators = [] }
          }
    end)

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

module Target = Build_interpret.Target
module Pre_rule = Build_interpret.Rule

let get_file : type a. t -> Path.t -> a File_kind.t -> a File_spec.t = fun t fn kind ->
  match Path.Table.find t.files fn with
  | None -> die "no rule found for %s" (Path.to_string fn)
  | Some (File_spec.T file) ->
    let Type_eq.T = File_kind.eq_exn kind file.kind in
    file

let vfile_to_string (type a) (module K : Vfile_kind.S with type t = a) _fn x =
  K.to_string x

type bs = t

module Build_exec = struct
  open Build.Repr

  let exec (bs : bs) (t : ('a, 'b) Build.t) (x : 'a) : 'b * Deps.t =
    let rec exec
      : type a b. Deps.t ref -> (a, b) t -> a -> b = fun dyn_deps t x ->
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
      | Paths_for_rule _ -> x
      | Paths_glob state -> get_glob_result_exn state
      | Contents p -> Io.read_file p
      | Lines_of p -> Io.lines_of_file p
      | Vpath (Vspec.T (fn, kind)) ->
        let file : b File_spec.t = get_file bs fn (Sexp_file kind) in
        Option.value_exn file.data
      | Dyn_paths t ->
        let fns = exec dyn_deps t x in
        dyn_deps := Deps.add_paths !dyn_deps fns;
        x
      | Record_lib_deps _ -> x
      | Fail { fail } -> fail ()
      | If_file_exists (_, state) ->
        exec dyn_deps (get_if_file_exists_exn state) x
      | Catch (t, on_error) -> begin
          try
            exec dyn_deps t x
          with exn ->
            on_error exn
        end
      | Lazy_no_targets t ->
        exec dyn_deps (Lazy.force t) x
      | Env_var _ ->
        x
      | Memo m ->
        match m.state with
        | Evaluated (x, deps) ->
          dyn_deps := Deps.union !dyn_deps deps;
          x
        | Evaluating ->
          die "Dependency cycle evaluating memoized build arrow %s" m.name
        | Unevaluated ->
          m.state <- Evaluating;
          let dyn_deps' = ref Deps.empty in
          match exec dyn_deps' m.t x with
          | x ->
            m.state <- Evaluated (x, !dyn_deps');
            dyn_deps := Deps.union !dyn_deps !dyn_deps';
            x
          | exception exn ->
            m.state <- Unevaluated;
            reraise exn
    in
    let dyn_deps = ref Deps.empty in
    let result = exec dyn_deps (Build.repr t) x in
    (result, !dyn_deps)
end

(* [copy_source] is [true] for rules copying files from the source directory *)
let add_spec t fn spec ~copy_source =
  match Path.Table.find t.files fn with
  | None ->
    Path.Table.add t.files fn spec
  | Some (File_spec.T { rule; _ }) ->
    match copy_source, rule.mode with
    | true, (Standard | Not_a_rule_stanza) ->
      Errors.warn (Internal_rule.loc rule ~dir:(Path.parent_exn fn)
                  ~file_tree:t.file_tree)
        "File %s is both generated by a rule and present in the source tree.\n\
         As a result, the rule is currently ignored, however this will become an error \
         in the future.\n\
         %t"
        (String.maybe_quoted (Path.basename fn))
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
      Path.Table.add t.files fn spec
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
let pending_targets = ref Path.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
    let fns = !pending_targets in
    pending_targets := Path.Set.empty;
    Path.Set.iter fns ~f:Path.unlink_no_err)

let compute_targets_digest_after_rule_execution targets =
  let good, bad =
    List.partition_map targets ~f:(fun fn ->
      match Utils.Cached_digest.refresh fn with
      | digest -> Left digest
      | exception (Unix.Unix_error _ | Sys_error _) -> Right fn)
  in
  match bad with
  | [] -> Digest.string (Marshal.to_string good [])
  | missing ->
    die "@{<error>Error@}: Rule failed to generate the following targets:\n%s"
      (string_of_paths (Path.Set.of_list missing))

let make_local_dir t fn =
  if not (Path.Set.mem t.local_mkdirs fn) then begin
    Path.mkdir_p fn;
    t.local_mkdirs <- Path.Set.add t.local_mkdirs fn
  end

let make_local_dirs t paths =
  Path.Set.iter paths ~f:(make_local_dir t)

let make_local_parent_dirs_for t ~map_path path =
  let path = map_path path in
  if Path.is_managed path then
    Option.iter (Path.parent path) ~f:(make_local_dir t)

let make_local_parent_dirs t paths ~map_path =
  Path.Set.iter paths ~f:(make_local_parent_dirs_for t ~map_path)

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
    | files ->
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
                  Path.Set.mem t.build_dirs_to_keep path then ()
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
    | Alias (ctx, fn') ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        let fn = Path.append (Path.relative Path.build_dir ctx) fn' in
        die "Trying to build alias %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))

let rec compile_rule t ?(copy_source=false) pre_rule =
  let { Pre_rule.
        context
      ; env
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
  let static_deps =
    Fiber.Once.create (fun () ->
      Fiber.return
        (Build_interpret.static_deps build ~all_targets:(targets_of t)
           ~file_tree:t.file_tree))
  in
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
    ; loc
    ; dir
    ; transitive_rev_deps = Internal_rule.Id.Set.singleton id
    ; rev_deps = []
    }
  in
  create_file_specs t target_specs rule ~copy_source

and start_rule t _rule =
  t.hook Rule_started

and run_rule  t rule action deps =
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
      ; loc = _
      ; transitive_rev_deps = _
      ; rev_deps = _
      } = rule in
  make_local_dir t dir;
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
      ( Deps.trace deps env,
        List.map targets_as_list ~f:Path.to_string,
        Option.map context ~f:(fun c -> c.name),
        Action.for_shell action)
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
      Some (Path.relative sandbox_dir (Digest.to_string rule_digest))
    else
      None
  in
  let force =
    !Clflags.force &&
    List.exists targets_as_list ~f:Path.is_alias_stamp_file
  in
  let something_changed =
    match prev_trace, targets_digest with
    | Some prev_trace, Some targets_digest ->
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
        | Some sandbox_dir ->
          Path.rm_rf sandbox_dir;
          let sandboxed path = Path.sandbox_managed_paths ~sandbox_dir path in
          make_local_parent_dirs t (Deps.paths deps) ~map_path:sandboxed;
          make_local_dir t (sandboxed dir);
          Action.sandbox action
            ~sandboxed
            ~deps:deps
            ~targets:targets_as_list
        | None ->
          action
      in
      make_local_dirs t (Action.chdirs action);
      with_locks locks ~f:(fun () ->
        Action_exec.exec ~context ~env ~targets action)
      >>| fun () ->
      Option.iter sandbox_dir ~f:Path.rm_rf;
      (* All went well, these targets are no longer pending *)
      pending_targets := Path.Set.diff !pending_targets targets;
      let targets_digest =
        compute_targets_digest_after_rule_execution targets_as_list
      in
      Trace.set head_target { rule_digest; targets_digest }
    end else
      Fiber.return ()
  end >>| fun () ->
  begin
    match mode with
    | Standard | Fallback | Not_a_rule_stanza | Ignore_source_files -> ()
    | Promote | Promote_but_delete_on_clean ->
      Path.Set.iter targets ~f:(fun path ->
        let in_source_tree = Option.value_exn (Path.drop_build_context path) in
        if not (Path.exists in_source_tree) ||
           (Utils.Cached_digest.file path <>
            Utils.Cached_digest.file in_source_tree) then begin
          if mode = Promote_but_delete_on_clean then
            Promoted_to_delete.add in_source_tree;
          Scheduler.ignore_for_watch in_source_tree;
          Io.copy_file ~src:path ~dst:in_source_tree ()
        end)
  end;
  t.hook Rule_completed

and setup_copy_rules t ~ctx_dir ~non_target_source_files =
  Path.Set.iter non_target_source_files ~f:(fun path ->
    let ctx_path = Path.append ctx_dir path in
    let build = Build.copy ~src:path ~dst:ctx_path in
    (* We temporarily allow overrides while setting up copy rules from
       the source directory so that artifact that are already present
       in the source directory are not re-computed.

       This allows to keep generated files in tarballs. Maybe we
       should allow it on a case-by-case basis though. *)
    compile_rule t (Pre_rule.make build ~context:None ~env:None)
      ~copy_source:true)

and load_dir   t ~dir = ignore (load_dir_and_get_targets t ~dir : Path.Set.t)
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

and load_dir_step2_exn t ~dir ~collector ~lazy_generators =
  List.iter lazy_generators ~f:(fun f -> f ());

  let context_name, sub_dir = Option.value_exn (Path.extract_build_context dir) in

  (* Load all the rules *)
  let extra_subdirs_to_keep =
    if context_name = "install" then
      These String.Set.empty
    else
      let gen_rules = String.Map.find_exn t.gen_rules context_name in
      gen_rules ~dir (Option.value_exn (Path.explode sub_dir))
  in
  let rules = collector.rules in

  (* Compute alias rules *)
  let alias_dir = Path.append (Path.relative alias_dir context_name) sub_dir in
  let alias_rules, alias_stamp_files =
    let open Build.O in
    let aliases = collector.aliases in
    let aliases =
      if String.Map.mem collector.aliases "default" then
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
              ; actions = []
              }
    in
    String.Map.foldi aliases ~init:([], Path.Set.empty)
      ~f:(fun name { Dir_status. deps; dyn_deps; actions } (rules, alias_stamp_files) ->
        let base_path = Path.relative alias_dir name in
        let rules, deps =
          List.fold_left actions ~init:(rules, deps)
            ~f:(fun (rules, deps)
                 { Dir_status. stamp; action; locks ; context ; loc ; env } ->
                 let path =
                   Path.extend_basename base_path
                     ~suffix:("-" ^ Digest.to_string stamp)
                 in
                 let rule =
                   Pre_rule.make ~locks ~context:(Some context) ~env ?loc
                     (Build.progn [ action; Build.create_file path ])
                 in
                 (rule :: rules, Path.Set.add deps path))
        in
        let path = Path.extend_basename base_path ~suffix:Alias0.suffix in
        (Pre_rule.make
           ~context:None
           ~env:None
           (Build.path_set deps >>>
            dyn_deps >>>
            Build.dyn_path_set (Build.arr (fun x -> x))
            >>^ (fun dyn_deps ->
              let deps = Path.Set.union deps dyn_deps in
              Action.with_stdout_to path
                (Action.digest_files (Path.Set.to_list deps)))
            >>>
            Build.action_dyn () ~targets:[path])
         :: rules,
         Path.Set.add alias_stamp_files path))
  in
  Path.Table.replace t.dirs ~key:alias_dir ~data:(Loaded alias_stamp_files);

  (* Compute the set of targets and the set of source files that must not be copied *)
  let user_rule_targets, source_files_to_ignore =
    List.fold_left rules ~init:(Path.Set.empty, Path.Set.empty)
      ~f:(fun (acc_targets, acc_ignored) { Pre_rule.targets; mode; _ } ->
        let targets = Build_interpret.Target.paths targets in
        (Path.Set.union targets acc_targets,
         match mode with
         | Promote | Promote_but_delete_on_clean | Ignore_source_files ->
           Path.Set.union targets acc_ignored
         | _ ->
           acc_ignored))
  in
  let source_files_to_ignore =
    Path.Set.map source_files_to_ignore ~f:(fun p ->
      Option.value_exn (Path.drop_build_context p))
  in

  (* Take into account the source files *)
  let targets, to_copy, subdirs_to_keep =
    match context_name with
    | "install" ->
      (user_rule_targets,
       None,
       String.Set.empty)
    | ctx_name ->
      (* This condition is [true] because of [get_dir_status] *)
      assert (String.Map.mem t.contexts ctx_name);
      let files, subdirs =
        match File_tree.find_dir t.file_tree sub_dir with
        | None -> (Path.Set.empty, String.Set.empty)
        | Some dir ->
          (File_tree.Dir.file_paths    dir,
           File_tree.Dir.sub_dir_names dir)
      in
      let files = Path.Set.diff files source_files_to_ignore in
      if Path.Set.is_empty files then
        (user_rule_targets, None, subdirs)
      else
        let ctx_path = Path.(relative build_dir) context_name in
        (Path.Set.union user_rule_targets
           (Path.Set.map files ~f:(Path.append ctx_path)),
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
      List.filter rules ~f:(fun (rule : Build_interpret.Rule.t) ->
        match rule.mode with
        | Standard | Promote | Promote_but_delete_on_clean
        | Not_a_rule_stanza | Ignore_source_files -> true
        | Fallback ->
          let source_files_for_targtes =
            List.fold_left rule.targets ~init:Path.Set.empty
              ~f:(fun acc target ->
                Path.Set.add acc
                  (Build_interpret.Target.path target
                   |> Path.drop_build_context
                   (* All targets are in [dir] and we know it
                      correspond to a directory of a build context
                      since there are source files to copy, so this
                      call can't fail. *)
                   |> Option.value_exn))
          in
          if Path.Set.is_subset source_files_for_targtes ~of_:to_copy then
            (* All targets are present *)
            false
          else begin
            if Path.Set.is_empty (Path.Set.inter source_files_for_targtes to_copy) then
              (* No target is present *)
              true
            else begin
              let absent_targets =
                Path.Set.diff source_files_for_targtes to_copy
              in
              let present_targets =
                Path.Set.diff source_files_for_targtes absent_targets
              in
              Errors.fail
                (rule_loc
                   ~file_tree:t.file_tree
                   ~loc:rule.loc
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
  Path.Table.replace t.dirs ~key:dir ~data:(Loaded targets);
  (match t.load_dir_stack with
   | [] -> assert false
   | x :: l ->
     t.load_dir_stack <- l;
     assert (Path.equal x dir));

  (* Compile the rules and cleanup stale artifacts *)
  List.iter rules ~f:(compile_rule t ~copy_source:false);
  Option.iter to_copy ~f:(fun (ctx_dir, source_files) ->
    setup_copy_rules t ~ctx_dir ~non_target_source_files:source_files);
  remove_old_artifacts t ~dir ~subdirs_to_keep;

  List.iter alias_rules ~f:(compile_rule t ~copy_source:false);
  remove_old_artifacts t ~dir:alias_dir ~subdirs_to_keep;

  targets

let get_file_spec_other t fn =
  let dir = Path.parent_exn fn in
  if Path.is_in_build_dir dir then
    load_dir t ~dir;
  match Path.Table.find t.files fn with
  | Some file ->
    Fiber.return (Some file)
  | None ->
    Fiber.return None

and get_file_spec t path =
  match Path.Table.find t.files path with
  | Some _ as some -> Fiber.return some
  | None ->
    let dir = Path.parent_exn path in
    if Path.is_strict_descendant_of_build_dir dir then begin
      load_dir t ~dir;
      match Path.Table.find t.files path with
      | Some _ as some -> Fiber.return some
      | None ->
        let stack = Memo.get_call_stack () in
        let loc =
          List.find_map stack ~f:Rule_fn.Stack_frame.input
          |> Option.bind ~f:(fun rule -> rule.Internal_rule.loc)
        in
        no_rule_found t ~loc path
    end else if Path.exists path then
      Fiber.return None
    else
      die "File unavailable: %s" (Path.to_string_maybe_quoted path)

let stamp_file_for_files_of t ~dir ~ext =
  let files_of_dir =
    Path.Table.find_or_add t.files_of dir ~f:(fun dir ->
      let files_by_ext =
        targets_of t ~dir
        |> Path.Set.to_list
        |> List.map ~f:(fun fn -> Filename.extension (Path.to_string fn), fn)
        |> String.Map.of_list_multi
      in
      { files_by_ext
      ; dir_hash = Path.to_string dir |> Digest.string |> Digest.to_string
      ; stamps = String.Map.empty
      })
  in
  match String.Map.find files_of_dir.stamps ext with
  | Some fn -> fn
  | None ->
    let stamp_file = Path.relative misc_dir (files_of_dir.dir_hash ^ ext) in
    let files =
      Option.value
        (String.Map.find files_of_dir.files_by_ext ext)
        ~default:[]
    in
    compile_rule t
      (let open Build.O in
       Pre_rule.make
         ~env:None
         ~context:None
         (Build.paths files >>>
          Build.action ~targets:[stamp_file]
            (Action.with_stdout_to stamp_file
               (Action.digest_files files))));
    files_of_dir.stamps <- String.Map.add files_of_dir.stamps ext stamp_file;
    stamp_file

let all_targets t =
  String.Map.iter t.contexts ~f:(fun ctx ->
    File_tree.fold t.file_tree ~traverse_ignored_dirs:true ~init:()
      ~f:(fun dir () ->
        load_dir t
          ~dir:(Path.append ctx.Context.build_dir (File_tree.Dir.path dir))));
  Path.Table.foldi t.files ~init:[] ~f:(fun key _ acc -> key :: acc)

let universe_file = Path.relative Path.build_dir ".universe-state"

let update_universe t =
  (* To workaround the fact that [mtime] is not precise enough on OSX *)
  Utils.Cached_digest.remove universe_file;
  let n =
    if Path.exists universe_file then
      Dune_lang.Decoder.(parse int) Univ_map.empty
        (Dune_lang.Io.load ~mode:Single universe_file) + 1
    else
      0
  in
  make_local_dirs t (Path.Set.singleton Path.build_dir);
  Io.write_file universe_file (Dune_lang.to_string ~syntax:Dune (Dune_lang.Encoder.int n))

let parallel_iter_path_set deps ~f =
  Path.Set.to_list deps |> Fiber.parallel_iter ~f

let parallel_iter_deps deps ~f =
  Deps.paths deps |> parallel_iter_path_set ~f

let prepare_rule t (rule : Internal_rule.t) : Action_and_deps.t Fiber.t =
  (* get the static dependencies needed before we can call build exec*)
  Fiber.Once.get rule.static_deps
  >>| Static_deps.rule_deps
  >>= (fun rule_deps ->
    start_rule t rule; (* legacy for hooks *)
    (* first compute rule dependencies*)
    parallel_iter_deps rule_deps ~f:(fun f ->
      Path_fn.exec (Fdecl.get t.build_file_def) f)
    (* then execute the build arrow *)
  )
  >>| Build_exec.exec t rule.build

let build_rule t (rule : Internal_rule.t) =
  let build_file = Path_fn.exec (Fdecl.get t.build_file_def) in

  (* get the static dependencies needed before we can call build exec*)
  Fiber.Once.get rule.static_deps
  >>= (fun deps ->
    let action_deps = Static_deps.action_deps deps in
    let rule_deps = Static_deps.rule_deps deps in

    Fiber.fork (fun () -> prepare_rule t rule)
    >>= (fun rule_eval ->
      (* now compute the action dependencies *)
      Fiber.fork (fun () -> parallel_iter_deps action_deps ~f:build_file)
      (* wait for action dependencies *)
      >>= Fiber.Future.wait
      (* wait for rule dependencies *)
      >>> Fiber.Future.wait rule_eval
      (* wait for dynamic dependencies *)
      >>= fun (action, dyn_deps) ->
        Deps.path_diff dyn_deps rule_deps
        |> parallel_iter_path_set ~f:build_file
      >>> Fiber.return (action, Deps.union action_deps dyn_deps)
    )
  )

let build_rule_internal t (rule : Internal_rule.t) =
  Rule_fn.exec (Fdecl.get t.build_rule_def) rule
  >>= fun (action, all_deps) ->
  run_rule t rule action all_deps

(* a rule can have multiple files, but rule.run_rule may only be called once *)
let build_file t path =
  let on_error exn = Dep_path.reraise exn (Path path) in
  Fiber.with_error_handler ~on_error (fun () ->
    get_file_spec t path >>= function
    | None ->
      (* file already exists *)
      Fiber.return ()
    | Some (File_spec.T file) ->
      Fdecl.get t.build_rule_internal_def file.rule)

let build_request t static_only ~request =
  let result = Fdecl.create () in
  let request =
    let open Build.O in
    request >>^ fun res ->
    Fdecl.set result res;
    Action.Progn []
  in
  let static_deps =
    Fiber.Once.create (fun () ->
      Fiber.return (
        Build_interpret.static_deps
          request
          ~all_targets:(targets_of t)
          ~file_tree:t.file_tree))
  in
  let rule = Internal_rule.make_request ~build:request ~static_deps in
  (if static_only then prepare_rule else build_rule) t rule
  >>| fun (_act, deps) ->
  (Fdecl.get result, deps)

let process_memcycle t exn =
  let cycle =
    Memo.Cycle_error.get exn
    |> List.filter_map ~f:(fun frame ->
      if Path_fn.Stack_frame.instance_of frame ~of_:(Fdecl.get t.build_file_def)
      then
        Path_fn.Stack_frame.input frame
      else
        None)
  in
  let last = List.last cycle |> Option.value_exn in
  let first = List.hd cycle in
  let cycle = if last = first then cycle else last :: cycle in
  Exn.Fatal_error
    (Format.asprintf "Dependency cycle between the following files:\n    %s"
       (List.map cycle ~f:Path.to_string_maybe_quoted
        |> String.concat ~sep:"\n--> "))

let do_build (t : t) ~request =
  Hooks.End_of_build.once Promotion.finalize;
  update_universe t; (* ? *)
  (fun () -> build_request t false ~request:request)
  |> Fiber.with_error_handler ~on_error:(fun exn ->
    Dep_path.map exn ~f:(function
      | Memo.Cycle_error.E exn -> process_memcycle t exn
      | _ as exn -> exn
    ) |> raise
  )
  >>| (fun (res,_) -> res)

let create ~contexts ~file_tree ~hook =
  let contexts =
    List.map contexts ~f:(fun c -> (c.Context.name, c))
    |> String.Map.of_list_exn
  in
  let t =
    { contexts
    ; files      = Path.Table.create 1024
    ; packages   = Path.Table.create 1024
    ; local_mkdirs = Path.Set.empty
    ; dirs       = Path.Table.create 1024
    ; load_dir_stack = []
    ; file_tree
    ; gen_rules = String.Map.map contexts ~f:(fun _ ~dir:_ ->
        die "gen_rules called too early")
    ; build_dirs_to_keep = Path.Set.empty
    ; files_of = Path.Table.create 1024
    ; prefix = None
    ; hook
    ; build_rule_def = Fdecl.create ()
    ; build_file_def = Fdecl.create ()
    ; build_rule_internal_def = Fdecl.create ()
    ; prepare_rule_def = Fdecl.create ()
    }
  in
  Fdecl.set t.prepare_rule_def
    (Rule_fn.create "prepare-rule" (module Action_and_deps) (prepare_rule t)
       ~doc:"Evaluate the build arrow part of a rule."
     |> Rule_fn.exec);
  Fdecl.set t.build_rule_def
    (Rule_fn.create "build-rule" (module Action_and_deps) (build_rule t)
       ~doc:"Execute a rule.");
  Fdecl.set t.build_rule_internal_def
    (Rule_fn.create "build-rule-internal" (module Unit)
       (build_rule_internal t) ~doc:"-"
     |> Rule_fn.exec);
  Fdecl.set t.build_file_def
    (Path_fn.create "build-file" (module Unit) (build_file t)
       ~doc:"Build a file.");
  t

module Ir_set = Set.Make(Internal_rule)

let rules_for_files t paths =
  Path.Set.fold paths ~init:[] ~f:(fun path acc ->
    if Path.is_in_build_dir path then
      load_dir t ~dir:(Path.parent_exn path);
    match Path.Table.find t.files path with
    | None -> acc
    | Some (File_spec.T { rule; _ }) -> rule :: acc)
  |> Ir_set.of_list
  |> Ir_set.to_list

let rules_for_targets t targets =
  Internal_rule.Id.Top_closure_f.top_closure
    (rules_for_files t targets)
    ~key:(fun (r : Internal_rule.t) -> r.id)
    ~deps:(fun (r : Internal_rule.t) ->
      Fiber.Once.get r.static_deps
      >>| Static_deps.paths
      >>| rules_for_files t)
  >>| function
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun rule ->
         Path.to_string (Option.value_exn
                           (Path.Set.choose rule.Internal_rule.targets)))
       |> String.concat ~sep:"\n-> ")

let static_deps_of_request t request =
  Static_deps.paths @@
  Build_interpret.static_deps
    request
    ~all_targets:(targets_of t)
    ~file_tree:t.file_tree

let all_lib_deps t ~request =
  let targets = static_deps_of_request t request in
  rules_for_targets t targets >>= fun rules ->
  Fiber.parallel_map rules ~f:(fun rule ->
    Internal_rule.lib_deps rule >>| fun deps ->
    (rule, deps))
  >>| fun lib_deps ->
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
  |> String.Map.map ~f:(Path.Map.of_list_reduce ~f:Lib_deps_info.merge)

module Rule = struct
  module Id = Internal_rule.Id

  type t =
    { id      : Id.t
    ; dir     : Path.t
    ; deps    : Deps.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }

  let compare a b = Id.compare a.id b.id
end

module Rule_set = Set.Make(Rule)

let rules_for_files rules deps =
  Path.Set.fold (Deps.paths deps) ~init:Rule_set.empty ~f:(fun path acc ->
    match Path.Map.find rules path with
    | None -> acc
    | Some rule -> Rule_set.add acc rule)
  |> Rule_set.to_list

let build_rules_internal ?(recursive=false) t ~request =
  let rules = ref [] in
  let rec run_rule (rule : Internal_rule.t) =
    Fdecl.get t.prepare_rule_def rule
    >>= (fun (action,deps) ->
      let rule = {
        Rule.
        id = rule.id;
        dir = rule.dir;
        deps = deps;
        targets = rule.targets;
        context = rule.context;
        action = action;
      } in
      rules := rule :: !rules;
      if recursive then
        parallel_iter_deps deps ~f:proc_rule
      else
        Fiber.return ()
    )
  and proc_rule dep =
    get_file_spec_other t dep
    >>= (fun fs ->
      Option.value_exn fs |> fun (File_spec.T file) ->
      let rule = file.rule in
      run_rule rule)
    >>=
    Fiber.return in
  build_request t true ~request
  >>= (fun (_, deps) ->
    parallel_iter_deps deps ~f:proc_rule
    >>> Fiber.return deps)
  >>| (fun deps ->
    let targets = Build_interpret.static_deps
                    request
                    ~all_targets:(targets_of t)
                    ~file_tree:t.file_tree
                  |> Static_deps.rule_deps
                  |> Deps.path_diff deps
                  |> Deps.add_paths Deps.empty in
    let rules = !rules in
    let rules =
      List.fold_left rules ~init:Path.Map.empty ~f:(fun acc (r : Rule.t) ->
        Path.Set.fold r.targets ~init:acc ~f:(fun fn acc ->
          Path.Map.add acc fn r)) in
    match
      Rule.Id.Top_closure.top_closure
        (rules_for_files rules targets)
        ~key:(fun (r : Rule.t) -> r.id)
        ~deps:(fun (r : Rule.t) ->
          rules_for_files rules r.deps)
    with
    | Ok l -> l
    | Error cycle ->
      die "dependency cycle detected:\n   %s"
        (List.map cycle ~f:(fun rule ->
           Path.to_string (Option.value_exn (Path.Set.choose rule.Rule.targets)))
         |> String.concat ~sep:"\n-> ")
  )

let build_rules ?recursive t ~request =
  entry_point t ~f:(fun () ->
    build_rules_internal ?recursive t ~request)

let set_package t file package =
  Path.Table.add t.packages file package

let package_deps t pkg files =
  let rules_seen = ref Rule.Id.Set.empty in
  let rec loop fn acc =
    match Path.Table.find_all t.packages fn with
    | [] -> loop_deps fn acc
    | pkgs ->
      if List.mem pkg ~set:pkgs then
        loop_deps fn acc
      else
        List.fold_left pkgs ~init:acc ~f:add_package
  and add_package acc p =
    let open Package.Name.Infix in
    if p = pkg then
      acc
    else
      Package.Name.Set.add acc p
  and loop_deps fn acc =
    match Path.Table.find t.files fn with
    | None -> acc
    | Some (File_spec.T { rule = ir; _ }) ->
      if Rule.Id.Set.mem !rules_seen ir.id then
        acc
      else begin
        rules_seen := Rule.Id.Set.add !rules_seen ir.id;
        (* We know that at this point of execution, all the relevant
           ivars have been filled so the following calsl to
           [X.peek_exn] cannot raise. *)
        let static_deps = Fiber.Once.peek_exn ir.static_deps in
        let rule_deps = Static_deps.rule_deps static_deps in
        let _, deps = Rule_fn.peek_exn (Fdecl.get t.build_rule_def) ir in
        let dyn_deps = Deps.path_diff deps rule_deps in
        let action_deps =
          Static_deps.action_deps static_deps |> Deps.paths
        in
        Path.Set.fold (Path.Set.union action_deps dyn_deps)
          ~init:acc ~f:loop
      end
  in
  let open Build.O in
  Build.paths_for_rule files >>^ fun () ->
  (* We know that at this point of execution, all the relevant ivars
     have been filled *)
  Path.Set.fold files ~init:Package.Name.Set.empty ~f:loop_deps

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

let get_collector t ~dir =
  match get_dir_status t ~dir with
  | Collecting_rules collector ->
    if collector.rules = [] && String.Map.is_empty collector.aliases then
      add_build_dir_to_keep t ~dir;
    collector
  | Failed_to_load -> raise Already_reported
  | Loaded _ | Forward _ ->
    Exn.code_error
      (if Path.is_in_source_tree dir then
         "Build_system.get_collector called on source directory"
       else if Path.equal dir Path.build_dir then
         "Build_system.get_collector called on build_dir"
       else if not (Path.is_managed dir) then
         "Build_system.get_collector called on external directory"
       else
         "Build_system.get_collector called on closed directory")
      [ "dir", Path.to_sexp dir
      ; "load_dir_stack", Sexp.Encoder.list Path.to_sexp t.load_dir_stack
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
    Exn.code_error "Build_system.prefix_rules' prefix contains targets"
      ["targets", Path.Set.to_sexp (Build_interpret.Target.paths targets)]
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
       String.Map.is_empty collector.aliases then
      add_build_dir_to_keep t ~dir;
    p.lazy_generators <- f :: lazy_generators

let eval_glob t ~dir re =
  Path.Set.fold (targets_of t ~dir) ~init:[] ~f:(fun path acc ->
    let fn = Path.basename path in
    if Re.execp re fn then
      fn :: acc
    else
      acc)
  |> List.rev

module Alias = struct
  include Alias0

  let get_alias_def build_system t =
    let collector = get_collector build_system ~dir:t.dir in
    match String.Map.find collector.aliases t.name with
    | None ->
      let x =
        { Dir_status.
          deps     = Path.Set.empty
        ; dyn_deps = Build.return Path.Set.empty
        ; actions  = []
        }
      in
      collector.aliases <- String.Map.add collector.aliases t.name x;
      x
    | Some x -> x

  let add_deps build_system t ?dyn_deps deps =
    let def = get_alias_def build_system t in
    def.deps <- Path.Set.union def.deps deps;
    match dyn_deps with
    | None -> ()
    | Some build ->
      let open Build.O in
      def.dyn_deps <-
        Build.fanout def.dyn_deps build >>^ fun (a, b) ->
        Path.Set.union a b

  let add_action build_system t ~context ~env ~loc ?(locks=[]) ~stamp action =
    let def = get_alias_def build_system t in
    def.actions <- { stamp = Digest.string (Marshal.to_string stamp [])
                   ; action
                   ; locks
                   ; context
                   ; loc
                   ; env
                   } :: def.actions
end

let is_target t file =
  Path.Set.mem (targets_of t ~dir:(Path.parent_exn file)) file
