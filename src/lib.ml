open Import
open Result.O

(* +-----------------------------------------------------------------+
   | Raw library information                                         |
   +-----------------------------------------------------------------+ *)

module Status = struct
  type t =
    | Installed
    | Public  of Package.t
    | Private of Jbuild.Scope_info.Name.t

  let pp ppf t =
    Format.pp_print_string ppf
      (match t with
       | Installed -> "installed"
       | Public _ -> "public"
       | Private s ->
         sprintf "private (%s)" (Jbuild.Scope_info.Name.to_string s))

  let is_private = function
    | Private _ -> true
    | Installed | Public _ -> false
end

module Info = struct
  module Deps = struct
    type t =
      | Simple  of (Loc.t * string) list
      | Complex of Jbuild.Lib_dep.t list

    let of_lib_deps deps =
      let rec loop acc (deps : Jbuild.Lib_dep.t list) =
        match deps with
        | []               -> Some (List.rev acc)
        | Direct x :: deps -> loop (x :: acc) deps
        | Select _ :: _    -> None
      in
      match loop [] deps with
      | Some l -> Simple l
      | None   -> Complex deps

    let to_lib_deps = function
      | Simple  l -> List.map l ~f:Jbuild.Lib_dep.direct
      | Complex l -> l
  end

  type t =
    { loc              : Loc.t
    ; kind             : Jbuild.Library.Kind.t
    ; status           : Status.t
    ; src_dir          : Path.t
    ; obj_dir          : Path.t
    ; version          : string option
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; foreign_archives : Path.t list Mode.Dict.t
    ; jsoo_runtime     : Path.t list
    ; requires         : Deps.t
    ; ppx_runtime_deps : (Loc.t * string) list
    ; pps              : (Loc.t * Jbuild.Pp.t) list
    ; optional         : bool
    ; virtual_deps     : (Loc.t * string) list
    ; sub_systems      : Jbuild.Sub_system_info.t Sub_system_name.Map.t
    }

  let user_written_deps t =
    List.fold_left (t.virtual_deps @ t.ppx_runtime_deps)
      ~init:(Deps.to_lib_deps t.requires)
      ~f:(fun acc s -> Jbuild.Lib_dep.Direct s :: acc)

  let of_library_stanza ~dir (conf : Jbuild.Library.t) =
    let archive_file ext = Path.relative dir (conf.name ^ ext) in
    let archive_files ~f_ext =
      Mode.Dict.of_func (fun ~mode -> [archive_file (f_ext mode)])
    in
    let stubs =
      if Jbuild.Library.has_stubs conf then
        [Jbuild.Library.stubs_archive conf ~dir ~ext_lib:""]
      else
        []
    in
    let jsoo_runtime =
      List.map conf.buildable.js_of_ocaml.javascript_files
        ~f:(Path.relative dir)
    in
    let status =
      match conf.public with
      | None   -> Status.Private conf.scope_name
      | Some p -> Public p.package
    in
    let foreign_archives =
      { Mode.Dict.
        byte   = stubs
      ; native = Path.relative dir conf.name :: stubs
      }
    in
    { loc = conf.buildable.loc
    ; kind     = conf.kind
    ; src_dir  = dir
    ; obj_dir  = Utils.library_object_directory ~dir conf.name
    ; version  = None
    ; synopsis = conf.synopsis
    ; archives = archive_files ~f_ext:Mode.compiled_lib_ext
    ; plugins  = archive_files ~f_ext:Mode.plugin_ext
    ; optional = conf.optional
    ; foreign_archives
    ; jsoo_runtime
    ; status
    ; virtual_deps     = conf.virtual_deps
    ; requires         = Deps.of_lib_deps conf.buildable.libraries
    ; ppx_runtime_deps = conf.ppx_runtime_libraries
    ; pps = Jbuild.Preprocess_map.pps conf.buildable.preprocess
    ; sub_systems = conf.sub_systems
    }

  let of_findlib_package pkg =
    let module P = Findlib.Package in
    let loc = Loc.in_file (Path.to_string (P.meta_file pkg)) in
    let add_loc x = (loc, x) in
    let sub_systems =
      match P.dune_file pkg with
      | None -> Sub_system_name.Map.empty
      | Some fn ->
        Installed_dune_file.load ~fname:(Path.to_string fn)
    in
    { loc              = loc
    ; kind             = Normal
    ; src_dir          = P.dir pkg
    ; obj_dir          = P.dir pkg
    ; version          = P.version pkg
    ; synopsis         = P.description pkg
    ; archives         = P.archives pkg
    ; plugins          = P.plugins pkg
    ; jsoo_runtime     = P.jsoo_runtime pkg
    ; requires         = Simple (List.map (P.requires pkg) ~f:add_loc)
    ; ppx_runtime_deps = List.map (P.ppx_runtime_deps pkg) ~f:add_loc
    ; pps              = []
    ; virtual_deps     = []
    ; optional         = false
    ; status           = Installed
    ; (* We don't know how these are named for external libraries *)
      foreign_archives = Mode.Dict.make_both []
    ; sub_systems      = sub_systems
    }
end

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Error0 = struct
  module Library_not_available = struct
    module Reason = struct
      module Hidden = struct
        type t =
          { name   : string
          ; path   : Path.t
          ; reason : string
          }
      end

      type t =
        | Not_found
        | Hidden of Hidden.t

      let to_string = function
        | Not_found -> "not found"
        | Hidden { path; reason; _ } ->
          sprintf "in %s is hidden (%s)"
            (Path.to_string_maybe_quoted path) reason

      let pp ppf t = Format.pp_print_string ppf (to_string t)
    end

    type t =
      { loc    : Loc.t
      ; name   : string
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select = struct
    type t = { loc : Loc.t }
  end
end

module Resolved_select = struct
  type t =
    { src_fn : (string, Error0.No_solution_found_for_select.t) result
    ; dst_fn : string
    }
end

type sub_system = ..

module Sub_system0 = struct
  module type S = sig
    type t
    type sub_system += T of t
    val to_sexp : (t -> Syntax.Version.t * Sexp.t) option
  end

  type 'a s = (module S with type t = 'a)

  module Instance = struct
    type t = T : 'a s * 'a -> t
  end
end

module Id = struct
  type t =
    { unique_id : int
    ; path      : Path.t
    ; name      : string
    }
end

type t =
  { loc               : Loc.t
  ; name              : string
  ; unique_id         : int
  ; kind              : Jbuild.Library.Kind.t
  ; status            : Status.t
  ; src_dir           : Path.t
  ; obj_dir           : Path.t
  ; version           : string option
  ; synopsis          : string option
  ; archives          : Path.t list Mode.Dict.t
  ; plugins           : Path.t list Mode.Dict.t
  ; foreign_archives  : Path.t list Mode.Dict.t
  ; jsoo_runtime      : Path.t list
  ; requires          : t list Or_exn.t
  ; ppx_runtime_deps  : t list Or_exn.t
  ; pps               : t list Or_exn.t
  ; resolved_selects  : Resolved_select.t list
  ; optional          : bool
  ; user_written_deps : Jbuild.Lib_deps.t
  ; (* This is mutable to avoid this error:

       {[
         This kind of expression is not allowed as right-hand side of `let rec'
       }]
       *)
    mutable sub_systems : Sub_system0.Instance.t Lazy.t Sub_system_name.Map.t
  }

and db =
  { parent  : db option
  ; resolve : string -> resolve_result
  ; table   : (string, status) Hashtbl.t
  ; all     : string list Lazy.t
  }

and status =
  | St_initializing of Id.t (* To detect cycles *)
  | St_found        of t
  | St_not_found
  | St_hidden       of t * Error0.Library_not_available.Reason.Hidden.t

and error =
  | Library_not_available        of Error0.Library_not_available.t
  | No_solution_found_for_select of Error0.No_solution_found_for_select.t
  | Dependency_cycle             of (Path.t * string) list
  | Conflict                     of conflict
  | Overlap                      of overlap
  | Private_deps_not_allowed     of private_deps_not_allowed

and resolve_result =
  | Not_found
  | Found    of Info.t
  | Hidden   of Info.t * string
  | Redirect of db option * string

and conflict =
  { lib1 : t * Dep_path.Entry.t list
  ; lib2 : t * Dep_path.Entry.t list
  }

and overlap =
  { in_workspace : t
  ; installed    : t * Dep_path.Entry.t list
  }

and private_deps_not_allowed =
  { private_dep    : t
  ; pd_loc         : Loc.t
  }

type lib = t

module Error = struct
  include Error0

  module Conflict = struct
    type nonrec t = conflict =
      { lib1 : t * Dep_path.Entry.t list
      ; lib2 : t * Dep_path.Entry.t list
      }
  end

  module Overlap = struct
    type nonrec t = overlap =
      { in_workspace : t
      ; installed    : t * Dep_path.Entry.t list
      }
  end

  module Private_deps_not_allowed = struct
    type nonrec t = private_deps_not_allowed =
      { private_dep : t
      ; pd_loc      : Loc.t
      }
  end

  type t = error =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t
    | Overlap                      of Overlap.t
    | Private_deps_not_allowed     of Private_deps_not_allowed.t
end

exception Error of Error.t

let not_available ~loc reason fmt =
  Errors.kerrf fmt ~f:(fun s ->
    Loc.fail loc "%s %a" s
      Error.Library_not_available.Reason.pp reason)

(* +-----------------------------------------------------------------+
   | Generals                                                        |
   +-----------------------------------------------------------------+ *)

let name t = t.name

let kind         t = t.kind
let synopsis     t = t.synopsis
let archives     t = t.archives
let plugins      t = t.plugins
let jsoo_runtime t = t.jsoo_runtime
let unique_id    t = t.unique_id

let src_dir t = t.src_dir
let obj_dir t = t.obj_dir

let is_local t = Path.is_local t.obj_dir

let status t = t.status

let package t =
  match t.status with
  | Installed ->
    Some (Findlib.root_package_name t.name
          |> Package.Name.of_string)
  | Public p -> Some p.name
  | Private _ ->
    None

let to_id t : Id.t =
  { unique_id = t.unique_id
  ; path      = t.src_dir
  ; name      = t.name
  }

module Set = Set.Make(
struct
  type nonrec t = t
  let compare x y = compare x.unique_id y.unique_id
end)

module Map = Map.Make(
struct
  type nonrec t = t
  let compare x y = compare x.unique_id y.unique_id
end)

module L = struct
  type nonrec t = t list

  let to_iflags dirs =
    Arg_spec.S
      (Path.Set.fold dirs ~init:[] ~f:(fun dir acc ->
         Arg_spec.Path dir :: A "-I" :: acc)
       |> List.rev)

  let include_paths ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add acc (obj_dir t))
    in
    Path.Set.remove dirs stdlib_dir

  let include_flags ts ~stdlib_dir =
    to_iflags (include_paths ts ~stdlib_dir)

  let c_include_paths ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add acc t.src_dir)
    in
    Path.Set.remove dirs stdlib_dir

  let c_include_flags ts ~stdlib_dir =
    to_iflags (c_include_paths ts ~stdlib_dir)

  let link_flags ts ~mode ~stdlib_dir =
    Arg_spec.S
      (c_include_flags ts ~stdlib_dir ::
       List.map ts ~f:(fun t -> Arg_spec.Deps (Mode.Dict.get t.archives mode)))

  let compile_and_link_flags ~compile ~link ~mode ~stdlib_dir =
    let dirs =
      Path.Set.union
        (  include_paths compile ~stdlib_dir)
        (c_include_paths link    ~stdlib_dir)
    in
    Arg_spec.S
      (to_iflags dirs ::
       List.map link ~f:(fun t ->
         Arg_spec.Deps (Mode.Dict.get t.archives mode)))

  let jsoo_runtime_files ts =
    List.concat_map ts ~f:(fun t -> t.jsoo_runtime)

  let archive_files ts ~mode ~ext_lib =
    List.concat_map ts ~f:(fun t ->
      Mode.Dict.get t.archives mode @
      List.map (Mode.Dict.get t.foreign_archives mode)
        ~f:(Path.extend_basename ~suffix:ext_lib))

  let remove_dups l =
    let rec loop acc l seen =
      match l with
      | [] -> acc
      | x :: l ->
        if Int_set.mem seen x.unique_id then
          loop acc l seen
        else
          loop (x :: acc) l (Int_set.add seen x.unique_id)
    in
    loop [] l Int_set.empty
end

(* +-----------------------------------------------------------------+
   | Sub-systems                                                     |
   +-----------------------------------------------------------------+ *)

module Sub_system = struct
  type t = sub_system = ..

  module type S = sig
    module Info : Jbuild.Sub_system_info.S
    type t
    type sub_system += T of t
    val instantiate
      :  resolve:(Loc.t * string -> (lib, exn) result)
      -> get:(lib -> t option)
      -> lib
      -> Info.t
      -> t
    val to_sexp : (t -> Syntax.Version.t * Sexp.t) option
  end

  module type S' = sig
    include S
    val for_instance : t Sub_system0.s
    val get : lib -> t option
  end

  let all = Sub_system_name.Table.create ~default_value:None

  module Register(M : S) = struct
    let get lib =
      Option.map (Sub_system_name.Map.find lib.sub_systems M.Info.name)
        ~f:(fun (lazy (Sub_system0.Instance.T ((module X), t))) ->
          match X.T t with
          | M.T t -> t
          | _   -> assert false)

    let () =
      let module M = struct
        include M
        let for_instance = (module M : Sub_system0.S with type t = t)
        let get = get
      end in
      Sub_system_name.Table.set all ~key:M.Info.name
        ~data:(Some (module M : S'))
  end

  let requires_installed_dune_file name =
    match Sub_system_name.Table.get all name with
    | None -> false
    | Some (module M : S') ->
      Option.is_some M.to_sexp

  let instantiate name info lib ~resolve =
    let impl = Option.value_exn (Sub_system_name.Table.get all name) in
    let (module M : S') = impl in
    match info with
    | M.Info.T info ->
      Sub_system0.Instance.T
        (M.for_instance, M.instantiate ~resolve ~get:M.get lib info)
    | _ -> assert false

  let dump_config lib =
    Sub_system_name.Map.filter_map lib.sub_systems ~f:(fun (lazy inst) ->
      let (Sub_system0.Instance.T ((module M), t)) = inst in
      match M.to_sexp with
      | None -> None
      | Some f -> Some (f t))
end

(* +-----------------------------------------------------------------+
   | Library name resolution and transitive closure                  |
   +-----------------------------------------------------------------+ *)

let gen_unique_id =
  let next = ref 0 in
  fun () ->
    let n = !next in
    next := n + 1;
    n

(* Dependency stack used while resolving the dependencies of a library
   that was just returned by the [resolve] callback *)
module Dep_stack = struct
  type t =
    { stack : Id.t list
    ; seen  : Int_set.t
    }

  let empty =
    { stack = []
    ; seen  = Int_set.empty
    }

  let to_required_by t ~stop_at =
    let stop_at = stop_at.stack in
    let rec loop acc l =
      if l == stop_at then
        List.rev acc
      else
        match l with
        | [] -> assert false
        | { Id.path; name; _ } :: l ->
          loop (Dep_path.Entry.Library (path, name) :: acc) l
    in
    loop [] t.stack

  let dependency_cycle t (last : Id.t) =
    assert (Int_set.mem t.seen last.unique_id);
    let rec build_loop acc stack =
      match stack with
      | [] -> assert false
      | (x : Id.t) :: stack ->
        let acc = (x.path, x.name) :: acc in
        if x.unique_id = last.unique_id then
          acc
        else
          build_loop acc stack
    in
    let loop = build_loop [(last.path, last.name)] t.stack in
    Error (Dependency_cycle loop)

  let create_and_push t name path =
    let unique_id = gen_unique_id () in
    let init = { Id. unique_id; name; path } in
    (init,
     { stack = init :: t.stack
     ; seen  = Int_set.add t.seen unique_id
     })

  let push t (x : Id.t) : (_, _) result =
    if Int_set.mem t.seen x.unique_id then
      Error (dependency_cycle t x)
    else
      Ok { stack = x :: t.stack
         ; seen  = Int_set.add t.seen x.unique_id
         }
end

let check_private_deps ~(lib : lib) ~loc ~allow_private_deps =
  if (not allow_private_deps) && Status.is_private lib.status then
    Result.Error (Error (
      Private_deps_not_allowed { private_dep = lib ; pd_loc = loc }))
  else
    Ok lib

let already_in_table (info : Info.t) name x =
  let to_sexp = Sexp.To_sexp.(pair Path.sexp_of_t string) in
  let sexp =
    match x with
    | St_initializing x ->
      Sexp.List [Sexp.unsafe_atom_of_string "Initializing";
                 Path.sexp_of_t x.path]
    | St_found t ->
      List [Sexp.unsafe_atom_of_string "Found";
            Path.sexp_of_t t.src_dir]
    | St_not_found ->
      Sexp.unsafe_atom_of_string "Not_found"
    | St_hidden (_, { path; reason; _ }) ->
      List [Sexp.unsafe_atom_of_string "Hidden";
            Path.sexp_of_t path; Sexp.atom reason]
  in
  Sexp.code_error
    "Lib_db.DB: resolver returned name that's already in the table"
    [ "name"            , Sexp.atom name
    ; "returned_lib"    , to_sexp (info.src_dir, name)
    ; "conflicting_with", sexp
    ]

let result_of_resolve_status = function
  | St_initializing _     -> assert false
  | St_found x            -> Ok x
  | St_not_found          -> Error Error.Library_not_available.Reason.Not_found
  | St_hidden (_, hidden) -> Error (Hidden hidden)

let rec instantiate db name (info : Info.t) ~stack ~hidden =
  let id, stack =
    Dep_stack.create_and_push stack name info.src_dir
  in
  Option.iter (Hashtbl.find db.table name) ~f:(fun x ->
    already_in_table info name x);
  (* Add [id] to the table, to detect loops *)
  Hashtbl.add db.table name (St_initializing id);

  let allow_private_deps = Status.is_private info.status in

  let requires, pps, resolved_selects =
    resolve_user_deps db info.requires ~allow_private_deps ~pps:info.pps ~stack
  in
  let ppx_runtime_deps =
    resolve_simple_deps db info.ppx_runtime_deps ~allow_private_deps ~stack
  in
  let map_error x =
    Result.map_error x ~f:(fun e ->
      Dep_path.prepend_exn e (Library (info.src_dir, name)))
  in
  let requires         = map_error requires         in
  let ppx_runtime_deps = map_error ppx_runtime_deps in
  let resolve (loc, name) =
    resolve_dep db name ~allow_private_deps ~loc ~stack in
  let t =
    { loc               = info.loc
    ; name              = name
    ; unique_id         = id.unique_id
    ; kind              = info.kind
    ; status            = info.status
    ; src_dir           = info.src_dir
    ; obj_dir           = info.obj_dir
    ; version           = info.version
    ; synopsis          = info.synopsis
    ; archives          = info.archives
    ; plugins           = info.plugins
    ; foreign_archives  = info.foreign_archives
    ; jsoo_runtime      = info.jsoo_runtime
    ; requires          = requires
    ; ppx_runtime_deps  = ppx_runtime_deps
    ; pps               = pps
    ; resolved_selects  = resolved_selects
    ; optional          = info.optional
    ; user_written_deps = Info.user_written_deps info
    ; sub_systems       = Sub_system_name.Map.empty
    }
  in
  t.sub_systems <-
    Sub_system_name.Map.mapi info.sub_systems ~f:(fun name info ->
      lazy (Sub_system.instantiate name info t ~resolve));

  let res =
    let hidden =
      match hidden with
      | None ->
        Option.some_if
          (info.optional &&
           not (Result.is_ok t.requires && Result.is_ok t.ppx_runtime_deps))
          "optional with unavailable dependencies"
      | Some _ -> hidden
    in
    match hidden with
    | None -> St_found t
    | Some reason ->
      St_hidden (t, { name; path = t.src_dir; reason })
  in
  Hashtbl.replace db.table ~key:name ~data:res;
  res

and find db name : (t, Error.Library_not_available.Reason.t) result =
  result_of_resolve_status (find_internal db name ~stack:Dep_stack.empty)

and find_even_when_hidden db name =
  match find_internal db name ~stack:Dep_stack.empty with
  | St_initializing _     -> assert false
  | St_found t            -> Some t
  | St_not_found          -> None
  | St_hidden (t, _)      -> Some t

and find_internal db name ~stack : status =
  match Hashtbl.find db.table name with
  | Some x -> x
  | None   -> resolve_name db name ~stack

and resolve_dep db name ~allow_private_deps ~loc ~stack : (t, exn) result =
  match find_internal db name ~stack with
  | St_initializing id ->
    Error (Dep_stack.dependency_cycle stack id)
  | St_found lib -> check_private_deps ~lib ~loc ~allow_private_deps
  | St_not_found ->
    Error (Error (Library_not_available { loc; name; reason = Not_found }))
  | St_hidden (_, hidden) ->
    Error (Error (Library_not_available { loc; name; reason = Hidden hidden }))

and resolve_name db name ~stack =
  match db.resolve name with
  | Redirect (db', name') -> begin
      let db' = Option.value db' ~default:db in
      match find_internal db' name' ~stack with
      | St_initializing _ as x -> x
      | x ->
        Hashtbl.add db.table name x;
        x
    end
  | Found info ->
    instantiate db name info ~stack ~hidden:None
  | Not_found ->
    let res =
      match db.parent with
      | None    -> St_not_found
      | Some db -> find_internal db name ~stack
    in
    Hashtbl.add db.table name res;
    res
  | Hidden (info, hidden) ->
    match
      match db.parent with
      | None    -> St_not_found
      | Some db -> find_internal db name ~stack
    with
    | St_found _ as x ->
      Hashtbl.add db.table name x;
      x
    | _ ->
      instantiate db name info ~stack ~hidden:(Some hidden)

and available_internal db name ~stack =
  match resolve_dep db name ~allow_private_deps:true ~loc:Loc.none ~stack with
  | Ok    _ -> true
  | Error _ -> false

and resolve_simple_deps db names ~allow_private_deps ~stack =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (loc, name) :: names ->
      resolve_dep db name ~allow_private_deps ~loc ~stack >>= fun x ->
      loop (x :: acc) names
  in
  loop [] names

and resolve_complex_deps db deps ~allow_private_deps ~stack =
  let res, resolved_selects =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_selects) dep ->
      let res, acc_selects =
        match (dep : Jbuild.Lib_dep.t) with
        | Direct (loc, name) ->
          let res =
            resolve_dep db name ~allow_private_deps ~loc ~stack >>| fun x -> [x]
          in
          (res, acc_selects)
        | Select { result_fn; choices; loc } ->
          let res, src_fn =
            match
              List.find_map choices ~f:(fun { required; forbidden; file } ->
                if String_set.exists forbidden
                     ~f:(available_internal db ~stack) then
                  None
                else
                  match
                    let deps =
                      String_set.fold required ~init:[] ~f:(fun x acc ->
                        (Loc.none, x) :: acc)
                    in
                    resolve_simple_deps ~allow_private_deps db deps ~stack
                  with
                  | Ok ts -> Some (ts, file)
                  | Error _ -> None)
            with
            | Some (ts, file) ->
              (Ok ts, Ok file)
            | None ->
              let e = { Error.No_solution_found_for_select.loc } in
              (Error (Error (No_solution_found_for_select e)),
               Error e)
          in
          (res, { Resolved_select. src_fn; dst_fn = result_fn } :: acc_selects)
      in
      let res =
        match res, acc_res with
        | Ok l, Ok acc -> Ok (List.rev_append l acc)
        | (Error _ as res), _
        | _, (Error _ as res) -> res
      in
      (res, acc_selects))
  in
  let res =
    match res with
    | Ok    l -> Ok (List.rev l)
    | Error _ -> res
  in
  (res, resolved_selects)

and resolve_deps db deps ~allow_private_deps ~stack =
  match (deps : Info.Deps.t) with
  | Simple  names ->
    (resolve_simple_deps db names ~allow_private_deps ~stack, [])
  | Complex names ->
    resolve_complex_deps ~allow_private_deps db names ~stack

and resolve_user_deps db deps ~allow_private_deps ~pps ~stack =
  let deps, resolved_selects =
    resolve_deps db deps ~allow_private_deps ~stack in
  let deps, pps =
    match pps with
    | [] -> (deps, Ok [])
    | first :: others as pps ->
      (* Location of the list of ppx rewriters *)
      let loc =
        let last = Option.value (List.last others) ~default:first in
        { (fst first) with stop = (fst last).stop }
      in
      let pps =
        let pps = (pps : (Loc.t * Jbuild.Pp.t) list :> (Loc.t * string) list) in
        resolve_simple_deps db pps ~allow_private_deps:true ~stack
        >>= fun pps ->
        closure_with_overlap_checks None pps ~stack
      in
      let deps =
        let rec check_runtime_deps acc pps = function
          | [] -> loop acc pps
          | lib :: ppx_rts ->
            check_private_deps ~lib ~loc ~allow_private_deps >>= fun rt ->
            check_runtime_deps (rt :: acc) pps ppx_rts
        and loop acc = function
          | [] -> Ok acc
          | pp :: pps ->
            pp.ppx_runtime_deps >>= fun rt_deps ->
            check_runtime_deps acc pps rt_deps
        in
        deps >>= fun deps ->
        pps  >>= fun pps  ->
        loop deps pps
      in
      (deps, pps)
  in
  (deps, pps, resolved_selects)

and closure_with_overlap_checks db ts ~stack =
  let visited = ref String_map.empty in
  let res = ref [] in
  let orig_stack = stack in
  let rec loop t ~stack =
    match String_map.find !visited t.name with
    | Some (t', stack') ->
      if t.unique_id = t'.unique_id then
        Ok ()
      else
        let req_by = Dep_stack.to_required_by ~stop_at:orig_stack in
        Error
          (Error (Conflict { lib1 = (t', req_by stack')
                           ; lib2 = (t , req_by stack )
                           }))
    | None ->
      visited := String_map.add !visited t.name (t, stack);
      (match db with
       | None -> Ok ()
       | Some db ->
         match find_internal db t.name ~stack with
         | St_found t' ->
           if t.unique_id = t'.unique_id then
             Ok ()
           else begin
             let req_by = Dep_stack.to_required_by stack ~stop_at:orig_stack in
             Error
               (Error (Overlap
                         { in_workspace = t'
                         ; installed    = (t, req_by)
                         }))
           end
         | _ -> assert false)
      >>= fun () ->
      Dep_stack.push stack (to_id t) >>= fun stack ->
      t.requires >>= fun deps ->
      iter deps ~stack >>| fun () ->
      res := t :: !res
  and iter ts ~stack =
    match ts with
    | [] -> Ok ()
    | t :: ts ->
      loop t ~stack >>= fun () ->
      iter ts ~stack
  in
  iter ts ~stack >>| fun () ->
  List.rev !res

let closure_with_overlap_checks db l =
  closure_with_overlap_checks db l ~stack:Dep_stack.empty

let closure l = closure_with_overlap_checks None l

let to_exn res =
  match res with
  | Ok    x -> x
  | Error e -> raise e

let requires_exn         t = to_exn t.requires
let ppx_runtime_deps_exn t = to_exn t.ppx_runtime_deps
let closure_exn          l = to_exn (closure l)

module Compile = struct
  module Resolved_select = Resolved_select

  type nonrec t =
    { direct_requires   : t list Or_exn.t
    ; requires          : t list Or_exn.t
    ; pps               : t list Or_exn.t
    ; resolved_selects  : Resolved_select.t list
    ; optional          : bool
    ; user_written_deps : Jbuild.Lib_deps.t
    ; sub_systems       : Sub_system0.Instance.t Lazy.t Sub_system_name.Map.t
    }

  let for_lib db (t : lib) =
    { direct_requires   = t.requires
    ; requires          = t.requires >>= closure_with_overlap_checks db
    ; resolved_selects  = t.resolved_selects
    ; pps               = t.pps
    ; optional          = t.optional
    ; user_written_deps = t.user_written_deps
    ; sub_systems       = t.sub_systems
    }

  let direct_requires   t = t.direct_requires
  let requires          t = t.requires
  let resolved_selects  t = t.resolved_selects
  let pps               t = t.pps
  let optional          t = t.optional
  let user_written_deps t = t.user_written_deps
  let sub_systems t =
    Sub_system_name.Map.values t.sub_systems
    |> List.map ~f:(fun (lazy (Sub_system0.Instance.T ((module M), t))) ->
      M.T t)
end

(* +-----------------------------------------------------------------+
   | Databases                                                       |
   +-----------------------------------------------------------------+ *)

module DB = struct
  module Resolve_result = struct
    type t = resolve_result =
      | Not_found
      | Found    of Info.t
      | Hidden   of Info.t * string
      | Redirect of db option * string
  end

  type t = db

  let create ?parent ~resolve ~all () =
    { parent
    ; resolve
    ; table  = Hashtbl.create 1024
    ; all    = Lazy.from_fun all
    }

  let create_from_library_stanzas ?parent stanzas =
    let map =
      List.concat_map stanzas ~f:(fun (dir, (conf : Jbuild.Library.t)) ->
        let info = Info.of_library_stanza ~dir conf in
        match conf.public with
        | None ->
          [(conf.name, Resolve_result.Found info)]
        | Some p ->
          if p.name = conf.name then
            [(p.name, Found info)]
          else
            [ p.name   , Found info
            ; conf.name, Redirect (None, p.name)
            ])
      |> String_map.of_list
      |> function
      | Ok x -> x
      | Error (name, _, _) ->
        match
          List.filter_map stanzas ~f:(fun (_, (conf : Jbuild.Library.t)) ->
            if name = conf.name ||
               match conf.public with
               | None -> false
               | Some p -> name = p.name
            then Some conf.buildable.loc
            else None)
        with
        | [] | [_] -> assert false
        | loc1 :: loc2 :: _ ->
          die "Library %S is defined twice:\n\
               - %s\n\
               - %s"
            name
            (Loc.to_file_colon_line loc1)
            (Loc.to_file_colon_line loc2)
    in
    create () ?parent
      ~resolve:(fun name ->
        match String_map.find map name with
        | None   -> Not_found
        | Some x -> x)
      ~all:(fun () -> String_map.keys map)

  let create_from_findlib ?(external_lib_deps_mode=false) findlib =
    create ()
      ~resolve:(fun name ->
        match Findlib.find findlib name with
        | Ok pkg -> Found (Info.of_findlib_package pkg)
        | Error e ->
          match e with
          | Not_found ->
            if external_lib_deps_mode then
              Found
                (Info.of_findlib_package
                   (Findlib.dummy_package findlib ~name))
            else
              Not_found
          | Hidden pkg ->
            Hidden (Info.of_findlib_package pkg,
                    "unsatisfied 'exist_if'"))
      ~all:(fun () ->
        Findlib.all_packages findlib
        |> List.map ~f:Findlib.Package.name)

  let find = find
  let find_even_when_hidden = find_even_when_hidden

  let resolve t (loc, name) =
    match find t name with
    | Ok _ as res -> res
    | Error reason ->
      Error (Error (Library_not_available
                      { loc
                      ; name
                      ; reason
                      }))

  let find_many =
    let rec loop t acc = function
      | [] -> Ok (List.rev acc)
      | name :: names ->
        resolve t (Loc.none, name) >>= fun lib ->
        loop t (lib ::acc) names
    in
    fun t names -> loop t [] names

  let available t name = available_internal t name ~stack:Dep_stack.empty

  let get_compile_info t ?(allow_overlaps=false) name =
    match find_even_when_hidden t name with
    | None ->
      Sexp.code_error "Lib.DB.get_compile_info got library that doesn't exist"
        [ "name", Sexp.To_sexp.string name ]
    | Some lib ->
      let t = Option.some_if (not allow_overlaps) t in
      Compile.for_lib t lib

  let resolve_user_written_deps t ?(allow_overlaps=false) deps ~pps =
    let res, pps, resolved_selects =
      resolve_user_deps t (Info.Deps.of_lib_deps deps) ~pps
        ~stack:Dep_stack.empty ~allow_private_deps:true
    in
    let requires =
      res
      >>=
      closure_with_overlap_checks (Option.some_if (not allow_overlaps) t)
    in
    { Compile.
      direct_requires = res
    ; requires
    ; pps
    ; resolved_selects
    ; optional          = false
    ; user_written_deps = deps
    ; sub_systems       = Sub_system_name.Map.empty
    }

  let resolve_pps t pps =
    resolve_simple_deps t ~allow_private_deps:true
      (pps : (Loc.t *Jbuild.Pp.t) list :> (Loc.t * string) list)
      ~stack:Dep_stack.empty

  let rec all ?(recursive=false) t =
    let l =
      List.fold_left (Lazy.force t.all) ~f:(fun libs name ->
        match find t name with
        | Ok    x -> Set.add libs x
        | Error _ -> libs) ~init:Set.empty
    in
    match recursive, t.parent with
    | true, Some t -> Set.union (all ~recursive t) l
    | _ -> l
end

(* +-----------------------------------------------------------------+
   | META files                                                      |
   +-----------------------------------------------------------------+ *)

module Meta = struct
  let to_names ts =
    List.fold_left ts ~init:String_set.empty ~f:(fun acc t ->
      String_set.add acc t.name)

  (* For the deprecated method, we need to put all the runtime
     dependencies of the transitive closure.

     We need to do this because [ocamlfind ocamlc -package ppx_foo]
     will not look for the transitive dependencies of [foo], and the
     runtime dependencies might be attached to a dependency of [foo]
     rather than [foo] itself.

     Sigh... *)
  let ppx_runtime_deps_for_deprecated_method t =
    closure_exn [t]
    |> List.concat_map ~f:ppx_runtime_deps_exn
    |> to_names

  let requires         t = to_names (requires_exn         t)
  let ppx_runtime_deps t = to_names (ppx_runtime_deps_exn t)
end

(* +-----------------------------------------------------------------+
   | Error reporting                                                 |
   +-----------------------------------------------------------------+ *)

let report_lib_error ppf (e : Error.t) =
  match e with
  | Library_not_available { loc = _; name; reason } ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %S %a.@\n"
      name
      Error.Library_not_available.Reason.pp reason
  | Conflict { lib1 = (lib1, rb1); lib2 = (lib2, rb2) } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: Conflict between the following libraries:@,\
       - %S in %s@,\
      \    %a@,\
       - %S in %s@,\
      \    %a@,\
       This cannot work.@\n"
      lib1.name (Path.to_string_maybe_quoted lib1.src_dir)
      Dep_path.Entries.pp rb1
      lib2.name (Path.to_string_maybe_quoted lib2.src_dir)
      Dep_path.Entries.pp rb2
  | Overlap { in_workspace = lib1; installed = (lib2, rb2) } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: Conflict between the following libraries:@,\
       - %S in %s@,\
       - %S in %s@,\
      \    %a@,\
       This is not allowed.@\n"
      lib1.name (Path.to_string_maybe_quoted lib1.src_dir)
      lib2.name (Path.to_string_maybe_quoted lib2.src_dir)
      Dep_path.Entries.pp rb2
  | No_solution_found_for_select { loc } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: No solution found for this select form.\n"
      Loc.print loc
  | Dependency_cycle cycle ->
    Format.fprintf ppf
      "@{<error>Error@}: Dependency cycle detected between the \
       following libraries:@\n\
       @[<v>%a@]\n"
      (Format.pp_print_list (fun ppf (path, name) ->
         Format.fprintf ppf "-> %S in %s"
           name (Path.to_string_maybe_quoted path)))
      cycle
  | Private_deps_not_allowed t ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %S is private, it cannot be a dependency of \
       a public library.\nYou need to give %S a public name.\n"
      t.private_dep.name
      t.private_dep.name

let () =
  Report_error.register (fun exn ->
    match exn with
    | Error e ->
      let loc, hint =
        match e with
        | Library_not_available { loc; _ } ->
          (Some loc,
           match !Clflags.external_lib_deps_hint with
           | [] -> (* during bootstrap *) None
           | l ->
             Some (List.map l ~f:quote_for_shell |> String.concat ~sep:" "))
        | Private_deps_not_allowed t ->
          (Some t.pd_loc, None)
        | _ -> (None, None)
      in
      Some
        { Report_error.
          loc
        ; hint
        ; pp = (fun ppf -> report_lib_error ppf e)
        ; backtrace = false
        }
    | _ -> None)
