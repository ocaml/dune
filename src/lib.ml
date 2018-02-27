open Import
open Result.O

(* +-----------------------------------------------------------------+
   | Raw library information                                         |
   +-----------------------------------------------------------------+ *)

module Status = struct
  type t =
    | Installed
    | Public
    | Private of Jbuild.Scope_info.Name.t
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
    List.fold_left t.virtual_deps
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
      | Some _ -> Public
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
          ; info   : Info.t
          ; reason : string
          }
      end

      type t =
        | Not_found
        | Hidden of Hidden.t

      let to_string = function
        | Not_found -> "not found"
        | Hidden { info; reason; _ } ->
          sprintf "in %s is hidden (%s)"
            (Path.to_string_maybe_quoted info.src_dir) reason

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
    type t = T : 'a s * 'a Lazy.t -> t
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
  ; requires          : t list or_error
  ; ppx_runtime_deps  : t list or_error
  ; pps               : t list or_error
  ; resolved_selects  : Resolved_select.t list
  ; optional          : bool
  ; user_written_deps : Jbuild.Lib_deps.t
  ; sub_systems       : Sub_system0.Instance.t Sub_system_name.Map.t
  }

and db =
  { parent  : db option
  ; resolve : string -> (info_or_redirect,
                         Error0.Library_not_available.Reason.t) result
  ; table   : (string, resolve_status) Hashtbl.t
  ; all     : string list Lazy.t
  }

and resolve_status =
  | Initializing of Id.t
  | Done         of (t, Error0.Library_not_available.Reason.t) result

and error =
  | Library_not_available        of Error0.Library_not_available.t
  | No_solution_found_for_select of Error0.No_solution_found_for_select.t
  | Dependency_cycle             of (Path.t * string) list
  | Conflict                     of conflict

and info_or_redirect =
  | Info     of Info.t
  | Redirect of Loc.t * Path.t * string
  | Proxy    of t

and conflict =
  { lib1 : t * Dep_path.Entry.t list
  ; lib2 : t * Dep_path.Entry.t list
  }

and 'a or_error = ('a, exn) result

type lib = t

module Error = struct
  include Error0

  module Conflict = struct
    type nonrec t = conflict =
      { lib1 : t * Dep_path.Entry.t list
      ; lib2 : t * Dep_path.Entry.t list
      }
  end

  type t = error =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t
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

let src_dir t = t.src_dir
let obj_dir t = t.obj_dir

let is_local t = Path.is_local t.obj_dir

let status t = t.status

let to_id t : Id.t =
  { unique_id = t.unique_id
  ; path      = t.src_dir
  ; name      = t.name
  }

module L = struct
  type nonrec t = t list

  let include_paths ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add acc (obj_dir t))
    in
    Path.Set.remove dirs stdlib_dir

  let include_flags ts ~stdlib_dir =
    let dirs = include_paths ts ~stdlib_dir in
    Arg_spec.S (List.concat_map (Path.Set.to_list dirs) ~f:(fun dir ->
      [Arg_spec.A "-I"; Path dir]))

  let c_include_flags ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add acc t.src_dir)
    in
    let dirs = Path.Set.remove dirs stdlib_dir in
    Arg_spec.S (List.concat_map (Path.Set.to_list dirs) ~f:(fun dir ->
      [Arg_spec.A "-I"; Path dir]))

  let link_flags ts ~mode ~stdlib_dir =
    Arg_spec.S
      (c_include_flags ts ~stdlib_dir ::
       List.map ts ~f:(fun t -> Arg_spec.Deps (Mode.Dict.get t.archives mode)))

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
      -> Id.t
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
        ~f:(fun (Sub_system0.Instance.T ((module X), lazy t)) ->
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

  let instantiate_many sub_systems id ~resolve =
    Sub_system_name.Map.mapi sub_systems ~f:(fun name info ->
      let impl = Option.value_exn (Sub_system_name.Table.get all name) in
      let (module M : S') = impl in
      match info with
      | M.Info.T info ->
        Sub_system0.Instance.T
          (M.for_instance, lazy (M.instantiate ~resolve ~get:M.get id info))
      | _ -> assert false)

  let dump_config lib =
    Sub_system_name.Map.filter_map lib.sub_systems ~f:(fun inst ->
      let (Sub_system0.Instance.T ((module M), lazy t)) = inst in
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

let already_in_table (info : Info.t) name x =
  let to_sexp = Sexp.To_sexp.(pair Path.sexp_of_t string) in
  let sexp =
    match x with
    | Initializing x ->
      Sexp.List [Sexp.unsafe_atom_of_string "Initializing";
                 Path.sexp_of_t x.path]
    | Done (Ok t) -> List [Sexp.unsafe_atom_of_string "Ok";
                           Path.sexp_of_t t.src_dir]
    | Done (Error Not_found) -> Sexp.unsafe_atom_of_string "Not_found"
    | Done (Error (Hidden { info; reason; _ })) ->
      List [Sexp.unsafe_atom_of_string "Hidden";
            Path.sexp_of_t info.src_dir; Sexp.atom reason]
  in
  Sexp.code_error
    "Lib_db.DB: resolver returned name that's already in the table"
    [ "name"            , Sexp.atom name
    ; "returned_lib"    , to_sexp (info.src_dir, name)
    ; "conflicting_with", sexp
    ]

let map_find_result ~loc name res : (_, _) result =
  match res with
  | Ok _ as res -> res
  | Error reason ->
    Error (Error (Error.Library_not_available { loc; name; reason }))

let rec make db name (info : Info.t) (id : Id.t) ~stack =
  let requires, pps, resolved_selects =
    resolve_user_deps db info.requires ~pps:info.pps ~stack
  in
  let ppx_runtime_deps =
    resolve_simple_deps db info.ppx_runtime_deps ~stack
  in
  let map_error x =
    Result.map_error x ~f:(fun e ->
      Dep_path.prepend_exn e (Library (info.src_dir, name)))
  in
  let requires         = map_error requires         in
  let ppx_runtime_deps = map_error ppx_runtime_deps in
  let resolve (loc, name) =
    find_internal db name ~loc ~stack
  in
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
  ; sub_systems       = Sub_system.instantiate_many info.sub_systems id ~resolve
  }

and find db name =
  match Hashtbl.find db.table name with
  | Some (Initializing _) -> assert false
  | Some (Done x) -> x
  | None -> resolve_name db name ~stack:Dep_stack.empty

and find_internal db name ~loc ~stack : (_, _) result =
  match Hashtbl.find db.table name with
  | Some (Initializing init) ->
    Error (Dep_stack.dependency_cycle stack init)
  | Some (Done x) -> map_find_result ~loc name x
  | None -> map_find_result ~loc name (resolve_name db name ~stack)

and resolve_name db name ~stack =
  match db.resolve name with
  | Ok (Proxy t) ->
    let res = Ok t in
    Hashtbl.replace db.table ~key:name ~data:(Done res);
    res
  | Ok (Redirect (loc, path, name')) ->
    let init, stack =
      Dep_stack.create_and_push stack name path
    in
    Hashtbl.add db.table name (Initializing init);
    let res =
      match find_internal db name' ~loc ~stack with
      | Ok _ as res -> res
      | Error _ ->
        match Hashtbl.find db.table name' with
        | Some (Done res) -> res
        | _ -> assert false
    in
    Hashtbl.replace db.table ~key:name ~data:(Done res);
    res
  | Ok (Info info) ->
    let id, stack =
      Dep_stack.create_and_push stack name info.src_dir
    in
    Option.iter (Hashtbl.find db.table name) ~f:(fun x ->
      already_in_table info name x);
    (* Add [id] to the table, to detect loops *)
    Hashtbl.add db.table name (Initializing id);
    let t = make db name info id ~stack in
    let res =
      if not info.optional ||
         (Result.is_ok t.requires && Result.is_ok t.ppx_runtime_deps) then
        Ok t
      else
        Error
          (Error.Library_not_available.Reason.Hidden
             { name
             ; info
             ; reason = "optional with unavailable dependencies"
             })
    in
    Hashtbl.replace db.table ~key:name ~data:(Done res);
    res
  | Error reason as res ->
    let res =
      match db.parent with
      | None -> res
      | Some db ->
        let res' = find db name in
        match res' with
        | Ok _ -> res'
        | Error _ ->
          if reason = Not_found then
            res'
          else
            res
    in
    Hashtbl.add db.table name (Done res);
    res

and available_internal db name ~stack =
  match find_internal db name ~loc:Loc.none ~stack with
  | Ok    _ -> true
  | Error _ -> false

and resolve_simple_deps db names ~stack =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (loc, name) :: names ->
      find_internal db name ~loc ~stack >>= fun x ->
      loop (x :: acc) names
  in
  loop [] names

and resolve_complex_deps db deps ~stack =
  let res, resolved_selects =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_selects) dep ->
      let res, acc_selects =
        match (dep : Jbuild.Lib_dep.t) with
        | Direct (loc, name) ->
          let res =
            find_internal db name ~loc ~stack >>| fun x -> [x]
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
                    resolve_simple_deps db deps ~stack
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

and resolve_deps db deps ~stack =
  match (deps : Info.Deps.t) with
  | Simple  names -> (resolve_simple_deps  db names ~stack, [])
  | Complex names ->  resolve_complex_deps db names ~stack

and resolve_user_deps db deps ~pps ~stack =
  let deps, resolved_selects = resolve_deps db deps ~stack in
  let deps, pps =
    match pps with
    | [] -> (deps, Ok [])
    | pps ->
      let pps =
        let pps = (pps : (Loc.t * Jbuild.Pp.t) list :> (Loc.t * string) list) in
        resolve_simple_deps db pps ~stack >>= fun pps ->
        closure pps ~stack
      in
      let deps =
        let rec loop acc = function
          | [] -> Ok acc
          | pp :: pps ->
            pp.ppx_runtime_deps >>= fun rt_deps ->
            loop (List.rev_append rt_deps acc) pps
        in
        deps >>= fun deps ->
        pps  >>= fun pps  ->
        loop deps pps
      in
      (deps, pps)
  in
  (deps, pps, resolved_selects)

and closure ts ~stack =
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

let closure l = closure l ~stack:Dep_stack.empty

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
    { direct_requires   : t list or_error
    ; requires          : t list or_error
    ; pps               : t list or_error
    ; resolved_selects  : Resolved_select.t list
    ; optional          : bool
    ; user_written_deps : Jbuild.Lib_deps.t
    ; sub_systems       : Sub_system0.Instance.t Sub_system_name.Map.t
    }

  let make libs =
    { direct_requires   = libs
    ; requires          = libs >>= closure
    ; resolved_selects  = []
    ; pps               = Ok []
    ; optional          = false
    ; user_written_deps = []
    ; sub_systems       = Sub_system_name.Map.empty
    }

  let for_lib (t : lib) =
    { direct_requires   = t.requires
    ; requires          = t.requires >>= closure
    ; resolved_selects  = t.resolved_selects
    ; pps               = t.pps
    ; optional          = t.optional
    ; user_written_deps = t.user_written_deps
    ; sub_systems       = t.sub_systems
    }

  let for_hidden db hidden =
    let { Error.Library_not_available.Reason.Hidden. name; info; _ } =
      hidden
    in
    let error =
      Error (Library_not_available
               { loc = info.loc
               ; name
               ; reason = Hidden hidden
               })
    in
    let resolved_selects =
      match info.requires with
      | Simple _ -> []
      | Complex deps ->
        List.filter deps ~f:(fun dep ->
          match (dep : Jbuild.Lib_dep.t) with
          | Direct _ -> false
          | Select _ -> true)
      |> resolve_complex_deps db ~stack:Dep_stack.empty
      |> snd
    in
    let resolve (loc, name) =
      find_internal db name ~loc ~stack:Dep_stack.empty
    in
    { direct_requires   = Error error
    ; requires          = Error error
    ; pps               = Error error
    ; resolved_selects  = resolved_selects
    ; optional          = info.optional
    ; user_written_deps = Info.user_written_deps info
    ; sub_systems       = Sub_system.instantiate_many info.sub_systems
                            { Id. unique_id = gen_unique_id ()
                            ; name
                            ; path = info.src_dir
                            }
                            ~resolve
    }

  let direct_requires   t = t.direct_requires
  let requires          t = t.requires
  let resolved_selects  t = t.resolved_selects
  let pps               t = t.pps
  let optional          t = t.optional
  let user_written_deps t = t.user_written_deps
  let sub_systems t =
    Sub_system_name.Map.values t.sub_systems
    |> List.map ~f:(fun (Sub_system0.Instance.T ((module M), lazy t)) -> M.T t)
end

(* +-----------------------------------------------------------------+
   | Databases                                                       |
   +-----------------------------------------------------------------+ *)

module DB = struct
  module Info_or_redirect = struct
    type nonrec t = info_or_redirect =
      | Info     of Info.t
      | Redirect of Loc.t * Path.t * string
      | Proxy    of t
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
          [(conf.name, Info_or_redirect.Info info)]
        | Some p ->
          if p.name = conf.name then
            [(p.name, Info info)]
          else
            [ p.name   , Info info
            ; conf.name, Redirect (conf.buildable.loc, dir, p.name)
            ])
      |> String_map.of_list
      |> function
      | Ok x -> x
      | Error (name, x, y) ->
        let pr : Info_or_redirect.t -> string = function
          | Info      info       -> Loc.to_file_colon_line info.loc
          | Redirect (loc, _, _) -> Loc.to_file_colon_line loc
          | Proxy     t          -> Loc.to_file_colon_line t.loc
        in
        die "Library %S is defined twice:\n\
             - %s\n\
             - %s"
          name
          (pr x)
          (pr y)
    in
    create () ?parent
      ~resolve:(fun name ->
        match String_map.find map name with
        | None -> Error Not_found
        | Some x -> Ok x)
      ~all:(fun () -> String_map.keys map)

  let create_from_findlib findlib =
    create ()
      ~resolve:(fun name ->
        match Findlib.find findlib name with
        | Ok pkg -> Ok (Info_or_redirect.Info (Info.of_findlib_package pkg))
        | Error e ->
          match e with
          | Not_found -> Error Not_found
          | Hidden pkg ->
            Error
              (Hidden
                 { name   = Findlib.Package.name pkg
                 ; info   = Info.of_findlib_package pkg
                 ; reason = "unsatisfied 'exist_if'"
                 }))
      ~all:(fun () ->
        Findlib.all_packages findlib
        |> List.map ~f:Findlib.Package.name)

  let find = find

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

  let get_compile_info t name =
    match find t name with
    | Error Not_found ->
      Sexp.code_error "Lib.DB.get_compile_info got library that doesn't exist"
        [ "name", Sexp.To_sexp.string name ]
    | Error (Hidden hidden) -> Compile.for_hidden t hidden
    | Ok lib -> Compile.for_lib lib

  let resolve_user_written_deps t deps ~pps =
    let res, pps, resolved_selects =
      resolve_user_deps t (Info.Deps.of_lib_deps deps) ~pps
        ~stack:Dep_stack.empty
    in
    { Compile.
      direct_requires = res
    ; requires        = res >>= closure
    ; pps
    ; resolved_selects
    ; optional          = false
    ; user_written_deps = deps
    ; sub_systems       = Sub_system_name.Map.empty
    }

  let resolve_pps t pps =
    resolve_simple_deps t
      (pps : (Loc.t *Jbuild.Pp.t) list :> (Loc.t * string) list)
      ~stack:Dep_stack.empty

  let rec all ?(recursive=false) t =
    let l =
      List.filter_map (Lazy.force t.all) ~f:(fun name ->
        match find t name with
        | Ok    x -> Some x
        | Error _ -> None)
    in
    match recursive, t.parent with
    | true, Some t -> all ~recursive t @ l
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
      "@[<v>@{<error>Error@}: Conflict between the following libaries:@,\
       - %S in %s@,\
      \    %a@,\
       - %S in %s@,\
      \    %a@,\
       This cannot work.@\n"
      lib1.name (Path.to_string_maybe_quoted lib1.src_dir)
      Dep_path.Entries.pp rb1
      lib2.name (Path.to_string_maybe_quoted lib2.src_dir)
      Dep_path.Entries.pp rb2
  | No_solution_found_for_select { loc } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: No solution found for this select form.\n"
      Loc.print loc
  | Dependency_cycle cycle ->
    Format.fprintf ppf
      "@{<error>Error@}: Dependency cycle detected between the \
       following libraries:\n\
       @[<v>%a@]\n"
      (Format.pp_print_list (fun ppf (path, name) ->
         Format.fprintf ppf "-> %S in %s"
           name (Path.to_string_maybe_quoted path)))
      cycle

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
