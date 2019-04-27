open Import
open! Stdune
open Result.O

(* Types *)

module Error = struct
  module Library_not_available = struct
    module Reason = struct
      module Hidden = struct
        type t =
          { name   : Lib_name.t
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
      ; name   : Lib_name.t
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select = struct
    type t = { loc : Loc.t }
  end

  module Double_implementation = struct
    type t =
      { impl1 : Lib_info.t * Dep_path.Entry.t list
      ; impl2 : Lib_info.t * Dep_path.Entry.t list
      ; vlib  : Lib_name.t
      }
  end

  module No_implementation = struct
    type t =
      { for_vlib : Lib_info.t * Dep_path.Entry.t list
      }
  end

  module Conflict = struct
    type t =
      { lib1 : Lib_info.t * Dep_path.Entry.t list
      ; lib2 : Lib_info.t * Dep_path.Entry.t list
      }
  end

  module Overlap = struct
    type t =
      { in_workspace : Lib_info.t
      ; installed    : Lib_info.t * Dep_path.Entry.t list
      }
  end

  module Multiple_implementations_for_virtual_lib = struct
    type t =
      { lib             : Lib_info.t
      ; loc             : Loc.t
      ; given_variants  : Variant.Set.t
      ; conflict        : Lib_info.t list
      }
  end

  module Private_deps_not_allowed = struct
    type t =
      { private_dep : Lib_info.t
      ; loc         : Loc.t
      }
  end

  module Not_virtual_lib = struct
    type t =
      { impl : Lib_info.t
      ; loc : Loc.t
      ; not_vlib : Lib_info.t
      }
  end

  module Default_implementation_cycle = struct
    type t =
      { cycle : Lib_info.t list
      }
  end

  type t =
    | Library_not_available                  of Library_not_available.t
    | No_solution_found_for_select           of No_solution_found_for_select.t
    | Dependency_cycle                       of (Path.t * Lib_name.t) list
    | Conflict                               of Conflict.t
    | Overlap                                of Overlap.t
    | Private_deps_not_allowed               of Private_deps_not_allowed.t
    | Double_implementation                  of Double_implementation.t
    | No_implementation                      of No_implementation.t
    | Not_virtual_lib                        of Not_virtual_lib.t
    | Multiple_implementations_for_virtual_lib
      of Multiple_implementations_for_virtual_lib.t
    | Default_implementation_cycle           of Default_implementation_cycle.t
end

exception Error of Error.t

module Resolved_select = struct
  type t =
    { src_fn : (string, Error.No_solution_found_for_select.t) result
    ; dst_fn : string
    }
end

type sub_system = ..

module Sub_system0 = struct
  module type S = sig
    type t
    type sub_system += T of t
    val encode : (t -> Syntax.Version.t * Dune_lang.t list) option
  end

  type 'a s = (module S with type t = 'a)

  module Instance = struct
    type t = T : 'a s * 'a -> t
  end
end

module Id : sig
  type t =
    { unique_id : int
    ; path      : Path.t
    ; name      : Lib_name.t
    }

  val hash : t -> int

  val compare : t -> t -> Ordering.t

  include Comparable.OPS with type t := t

  val make : path:Path.t -> name:Lib_name.t -> t

  module Set : Set.S with type elt = t

  module Top_closure : Top_closure.S
    with type key := t
     and type 'a monad := 'a Monad.Id.t
end = struct
  type t =
    { unique_id : int
    ; path      : Path.t
    ; name      : Lib_name.t
    }

  let compare t1 t2 = Int.compare t1.unique_id t2.unique_id

  include (
    Comparable.Operators(struct type nonrec t = t let compare = compare end)
    : Comparable.OPS with type t := t
  )

  let gen_unique_id =
    let next = ref 0 in
    fun () ->
      let n = !next in
      next := n + 1;
      n

  let hash t = t.unique_id

  let make ~path ~name =
    { unique_id = gen_unique_id ()
    ; path
    ; name
    }

  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

  module Top_closure = Top_closure.Make(Set)(Monad.Id)
end

module T = struct
  type t =
    { info              : Lib_info.t
    ; name              : Lib_name.t
    ; unique_id         : Id.t
    ; requires          : t list Or_exn.t
    ; ppx_runtime_deps  : t list Or_exn.t
    ; pps               : t list Or_exn.t
    ; resolved_selects  : Resolved_select.t list
    ; user_written_deps : Dune_file.Lib_deps.t
    ; implements        : t Or_exn.t option
    ; (* these fields cannot be forced until the library is instantiated *)
      default_implementation : t Or_exn.t Lazy.t option
    ; implementations   : t Or_exn.t list Variant.Map.t Lazy.t option
    ; (* This is mutable to avoid this error:

         {[
           This kind of expression is not allowed as right-hand side of `let rec'
         }]
         *)
      mutable sub_systems : Sub_system0.Instance.t Lazy.t Sub_system_name.Map.t
    }

  let compare (x : t) (y : t) = Id.compare x.unique_id y.unique_id
end

include T

include (Comparable.Operators(T) : Comparable.OPS with type t := t)

type status =
  | St_initializing of Id.t (* To detect cycles *)
  | St_found        of t
  | St_not_found
  | St_hidden       of t * Error.Library_not_available.Reason.Hidden.t

type db =
  { parent               : db option
  ; resolve              : Lib_name.t -> resolve_result
  ; find_implementations : Lib_name.t -> Lib_info.t list Variant.Map.t
  ; table                : (Lib_name.t, status) Hashtbl.t
  ; all                  : Lib_name.t list Lazy.t
  }

and resolve_result =
  | Not_found
  | Found    of Lib_info.t
  | Hidden   of Lib_info.t * string
  | Redirect of db option * Lib_name.t

type lib = t

let to_dyn t = Lib_name.to_dyn t.name

let not_available ~loc reason fmt =
  Errors.kerrf fmt ~f:(fun s ->
    Errors.fail loc "%s %a" s
      Error.Library_not_available.Reason.pp reason)

(* Generals *)

let name t = t.name

let kind         t = t.info.kind
let synopsis     t = t.info.synopsis
let archives     t = t.info.archives
let plugins      t = t.info.plugins
let jsoo_runtime t = t.info.jsoo_runtime
let jsoo_archive t = t.info.jsoo_archive
let unique_id    t = t.unique_id
let modes        t = t.info.modes

let virtual_     t = t.info.virtual_

let src_dir t = t.info.src_dir
let orig_src_dir t = Option.value ~default:t.info.src_dir t.info.orig_src_dir
let obj_dir t = t.info.obj_dir

let is_local t = Path.is_managed (Obj_dir.byte_dir t.info.obj_dir)

let public_cmi_dir t = Obj_dir.public_cmi_dir t.info.obj_dir

let native_dir t = Obj_dir.native_dir t.info.obj_dir

let status t = t.info.status

let foreign_objects t = t.info.foreign_objects

let main_module_name t =
  match t.info.main_module_name with
  | This mmn -> Ok mmn
  | From _ ->
    let+ vlib = Option.value_exn t.implements in
    match vlib.info.main_module_name with
    | This x -> x
    | From _ -> assert false

let wrapped t =
  match t.info.wrapped with
  | None -> Ok None
  | Some (This wrapped) -> Ok (Some wrapped)
  | Some (From _) ->
    let+ vlib = Option.value_exn t.implements in
    match vlib.info.wrapped with
    | Some (From _) (* can't inherit this value in virtual libs *)
    | None -> assert false (* will always be specified in dune package *)
    | Some (This x) -> Some x

let package t =
  match t.info.status with
  | Installed -> Some (Lib_name.package_name t.name)
  | Public p -> Some p.name
  | Private _ ->
    None

let to_id t : Id.t = t.unique_id
let equal l1 l2 = Id.equal (to_id l1) (to_id l2)
let hash t = Id.hash (to_id t)

module Set = Set.Make(T)
module Map = Map.Make(T)

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
        List.fold_left ~f:Path.Set.add ~init:acc
          [public_cmi_dir t ; native_dir t])
    in
    Path.Set.remove dirs stdlib_dir

  let include_flags ts ~stdlib_dir =
    to_iflags (include_paths ts ~stdlib_dir)

  let c_include_paths ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add acc t.info.src_dir)
    in
    Path.Set.remove dirs stdlib_dir

  let c_include_flags ts ~stdlib_dir =
    to_iflags (c_include_paths ts ~stdlib_dir)

  let link_flags ts ~mode ~stdlib_dir =
    Arg_spec.S
      (c_include_flags ts ~stdlib_dir ::
       List.map ts ~f:(fun t ->
         Arg_spec.Deps (Mode.Dict.get t.info.archives mode)))

  let compile_and_link_flags ~compile ~link ~mode ~stdlib_dir =
    let dirs =
      Path.Set.union
        (  include_paths compile ~stdlib_dir)
        (c_include_paths link    ~stdlib_dir)
    in
    Arg_spec.S
      (to_iflags dirs ::
       List.map link ~f:(fun t ->
         Arg_spec.Deps (Mode.Dict.get t.info.archives mode)))

  let jsoo_runtime_files ts =
    List.concat_map ts ~f:(fun t -> t.info.jsoo_runtime)

  let archive_files ts ~mode =
    List.concat_map ts ~f:(fun t ->
      Mode.Dict.get t.info.archives mode @
      Mode.Dict.get t.info.foreign_archives mode)

  let remove_dups l =
    let rec loop acc l seen =
      match l with
      | [] -> acc
      | x :: l ->
        if Id.Set.mem seen x.unique_id then
          loop acc l seen
        else
          loop (x :: acc) l (Id.Set.add seen x.unique_id)
    in
    loop [] l Id.Set.empty

  let top_closure l ~key ~deps =
    Id.Top_closure.top_closure l
      ~key:(fun t -> unique_id (key t))
      ~deps
end

module Lib_and_module = struct
  type nonrec t =
    | Lib of t
    | Module of Module.t

  let link_flags ts ~mode ~stdlib_dir =
    let libs = List.filter_map ts ~f:(function
      | Lib lib -> Some lib
      | Module _ -> None) in
    Arg_spec.S
      (L.c_include_flags libs ~stdlib_dir ::
       List.map ts ~f:(function
         | Lib t ->
           Arg_spec.Deps (Mode.Dict.get t.info.archives mode)
         | Module m ->
           Dep (Module.cm_file_unsafe m (Mode.cm_kind mode))
       ))

end

(* Sub-systems *)

module Sub_system = struct
  type t = sub_system = ..

  module type S = sig
    module Info : Sub_system_info.S
    type t
    type sub_system += T of t
    val instantiate
      :  resolve:(Loc.t * Lib_name.t -> lib Or_exn.t)
      -> get:(loc:Loc.t -> lib -> t option)
      -> lib
      -> Info.t
      -> t
    val encode : (t -> Syntax.Version.t * Dune_lang.t list) option
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

  let instantiate name info lib ~resolve =
    let impl = Option.value_exn (Sub_system_name.Table.get all name) in
    let (module M : S') = impl in
    match info with
    | M.Info.T info ->
      let get ~loc lib' =
        if lib = lib' then
          Errors.fail loc "Library %a depends on itself"
            Lib_name.pp_quoted lib.name
        else
          M.get lib'
      in
      Sub_system0.Instance.T
        (M.for_instance, M.instantiate ~resolve ~get lib info)
    | _ -> assert false

  let dump_config lib =
    Sub_system_name.Map.filter_map lib.sub_systems ~f:(fun (lazy inst) ->
      let (Sub_system0.Instance.T ((module M), t)) = inst in
      Option.map ~f:(fun f -> f t) M.encode)
end

(* Library name resolution and transitive closure *)

(* Dependency stack used while resolving the dependencies of a library
   that was just returned by the [resolve] callback *)
module Dep_stack = struct
  type t =
    { stack : Id.t list
    ; seen  : Id.Set.t
    }

  let empty =
    { stack = []
    ; seen  = Id.Set.empty
    }

  let to_required_by t ~stop_at =
    let stop_at = stop_at.stack in
    let rec loop acc l =
      if List.physically_equal l stop_at then
        List.rev acc
      else
        match l with
        | [] -> assert false
        | { Id.path; name; _ } :: l ->
          loop (Dep_path.Entry.Library (path, name) :: acc) l
    in
    loop [] t.stack

  let dependency_cycle t (last : Id.t) =
    assert (Id.Set.mem t.seen last);
    let rec build_loop acc stack =
      match stack with
      | [] -> assert false
      | (x : Id.t) :: stack ->
        let acc = (x.path, x.name) :: acc in
        if Id.equal x last then
          acc
        else
          build_loop acc stack
    in
    let loop = build_loop [last.path, last.name] t.stack in
    Error (Dependency_cycle loop)

  let create_and_push t name path =
    let init = Id.make ~path ~name in
    (init,
     { stack = init :: t.stack
     ; seen  = Id.Set.add t.seen init
     })

  let push t (x : Id.t) : (_, _) result =
    if Id.Set.mem t.seen x then
      Error (dependency_cycle t x)
    else
      Ok { stack = x :: t.stack
         ; seen  = Id.Set.add t.seen x
         }
end

let check_private_deps lib ~loc ~allow_private_deps =
  if (not allow_private_deps) && Lib_info.Status.is_private lib.info.status
  then
    Result.Error (Error (
      Private_deps_not_allowed { private_dep = lib.info; loc }))
  else
    Ok lib

let already_in_table (info : Lib_info.t) name x =
  let to_sexp = Sexp.Encoder.(pair Path.to_sexp Lib_name.to_sexp) in
  let sexp =
    match x with
    | St_initializing x ->
      Sexp.List [Sexp.Atom "Initializing";
                 Path.to_sexp x.path]
    | St_found t ->
      List [Sexp.Atom "Found";
            Path.to_sexp t.info.src_dir]
    | St_not_found ->
      Sexp.Atom "Not_found"
    | St_hidden (_, { path; reason; _ }) ->
      List [Sexp.Atom "Hidden";
            Path.to_sexp path; Sexp.Atom reason]
  in
  Exn.code_error
    "Lib_db.DB: resolver returned name that's already in the table"
    [ "name"            , Lib_name.to_sexp name
    ; "returned_lib"    , to_sexp (info.src_dir, name)
    ; "conflicting_with", sexp
    ]

let result_of_resolve_status = function
  | St_initializing _     -> assert false
  | St_found x            -> Ok x
  | St_not_found          -> Error Error.Library_not_available.Reason.Not_found
  | St_hidden (_, hidden) -> Error (Hidden hidden)

module Vlib : sig
  (** Make sure that for every virtual library in the list there is at
      most one corresponding implementation.

      Additionally, if linking is [true], ensures that every virtual
      library as an implementation and re-arrange the list so that
      implementations replaces virtual libraries. *)
  val associate
    :  (t * Dep_stack.t) list
    -> orig_stack:Dep_stack.t
    -> linking:bool
    -> t list Or_exn.t

  module Unimplemented : sig
    (** set of unimplemented libraries*)
    type t

    val empty : t

    val add : t -> lib -> t Or_exn.t

    val fold
      :  t
      -> init:'acc
      -> f:(lib -> 'acc -> 'acc Or_exn.t)
      -> 'acc Or_exn.t
  end
end = struct
  module Unimplemented = struct
    type status = Implemented | Not_implemented
    type t = status Map.t

    let empty = Map.empty

    let add t lib =
      match lib.implements, lib.info.virtual_ with
      | None, None -> Ok t
      | Some _, Some _ ->
        assert false (* can't be virtual and implement *)
      | None, Some _ ->
        Ok (Map.update t lib ~f:(function
          | None -> Some Not_implemented
          | Some _ as x -> x))
      | Some vlib, None ->
        let+ vlib = vlib in
        Map.add t vlib Implemented

    let fold =
      let rec loop ~f ~acc = function
        | [] -> Ok acc
        | (_, Implemented) :: libs -> loop ~f ~acc libs
        | (lib, Not_implemented) :: libs ->
          let* acc = f lib acc in
          loop ~f ~acc libs
      in
      fun t ~init ~f -> loop (Map.to_list t) ~acc:init ~f
  end
  module Table = struct
    module Partial = struct
      type vlib_status =
        | No_impl of Dep_stack.t
        | Impl of lib * Dep_stack.t
      type t = vlib_status Map.t

      let is_empty = Map.is_empty

      let make closure ~orig_stack : t Or_exn.t =
        let rec loop acc = function
          | [] -> Ok acc
          | (lib, stack) :: libs ->
            match lib.implements, lib.info.virtual_ with
            | None, None -> loop acc libs
            | Some _, Some _ ->
              assert false (* can't be virtual and implement *)
            | None, Some _ ->
              loop (Map.add acc lib (No_impl stack)) libs
            | Some vlib, None ->
              let* vlib = vlib in
              begin match Map.find acc vlib with
              | None ->
                (* we've already traversed the virtual library because
                   it must have occured earlier in the closure *)
                assert false
              | Some (No_impl _) ->
                loop (Map.add acc vlib (Impl (lib, stack))) libs
              | Some (Impl (lib', stack')) ->
                let req_by' =
                  Dep_stack.to_required_by stack' ~stop_at:orig_stack
                in
                let req_by =
                  Dep_stack.to_required_by stack ~stop_at:orig_stack
                in
                Error (Error (Double_implementation
                                { impl2 = (lib.info, req_by)
                                ; impl1 = (lib'.info, req_by')
                                ; vlib = vlib.name
                                }))
              end
        in
        loop Map.empty closure
    end

    type t = lib Map.t

    let make impls ~orig_stack : t Or_exn.t =
      let rec loop acc = function
        | [] -> Ok acc
        | (vlib, Partial.No_impl stack) :: _ ->
          let rb = Dep_stack.to_required_by stack ~stop_at:orig_stack in
          Error
            (Error
               (No_implementation { for_vlib = (vlib.info, rb) }))
        | (vlib, (Impl (impl, _stack))) :: libs ->
          loop (Map.add acc vlib impl) libs
      in
      loop Map.empty (Map.to_list impls)
  end

  let second_step_closure ts impls =
    let visited = ref Id.Set.empty in
    let res = ref [] in
    let rec loop t =
      let t = Option.value ~default:t (Map.find impls t) in
      if Id.Set.mem !visited t.unique_id then
        Ok ()
      else begin
        visited := Id.Set.add !visited t.unique_id;
        let* deps = t.requires in
        let+ () = Result.List.iter deps ~f:loop in
        res := t :: !res
      end
    in
    let+ () = Result.List.iter ts ~f:loop in
    List.rev !res

  let associate closure ~orig_stack ~linking =
    let* impls = Table.Partial.make closure ~orig_stack in
    let closure = List.map closure ~f:fst in
    if linking && not (Table.Partial.is_empty impls) then
      let* impls = Table.make impls ~orig_stack in
      second_step_closure closure impls
    else
      Ok closure
end

module Vlib_visit : sig
  type t

  val create : unit -> t

  val visit
    :  t
    -> lib
    -> stack:Lib_info.t list
    -> f:(lib -> unit Or_exn.t)
    -> unit Or_exn.t
end = struct
  module Status = struct
    type t =
      | Visiting
      | Visited
  end

  type t = Status.t Map.t ref

  let create () = ref Map.empty

  let visit t lib ~stack ~f =
    match Map.find !t lib with
    | Some Status.Visited -> Ok ()
    | Some Visiting ->
      Error (Error (Default_implementation_cycle
                      { cycle = (lib.info :: stack)
                      }))
    | None ->
      t := Map.add !t lib Visiting;
      let res = f lib in
      t := Map.add !t lib Visited;
      res
end

(* Find implementation that matches given variants *)
let find_implementation_for lib ~variants =
  match variants with
  | None -> Ok None
  | Some (loc, variants_set) ->
    begin match lib.implementations with
    | None -> Ok None (* shouldn't happen and yet it does.. *)
    | Some (lazy available_implementations) ->
      let* candidates =
        Variant.Set.fold variants_set
          ~init:[]
          ~f:(fun variant acc ->
            List.rev_append acc
              (Variant.Map.Multi.find available_implementations variant))
        |> Result.List.all
      in
      match candidates with
      | [] -> Ok None
      | [elem] -> Ok (Some elem)
      | conflict ->
        let conflict = List.map conflict ~f:(fun lib -> lib.info) in
        Error (Error (Multiple_implementations_for_virtual_lib
                        { lib = lib.info
                        ; loc
                        ; given_variants = variants_set
                        ; conflict
                        }))
    end

let rec instantiate db name (info : Lib_info.t) ~stack ~hidden =
  let id, stack =
    Dep_stack.create_and_push stack name info.src_dir
  in
  Option.iter (Hashtbl.find db.table name) ~f:(fun x ->
    already_in_table info name x);
  (* Add [id] to the table, to detect loops *)
  Hashtbl.add db.table name (St_initializing id);

  let allow_private_deps = Lib_info.Status.is_private info.status in

  let resolve (loc, name) =
    resolve_dep db (name : Lib_name.t) ~allow_private_deps ~loc ~stack in

  let implements =
    Option.map info.implements ~f:(fun ((loc,  _) as name) ->
      let* vlib = resolve name in
      match vlib.info.virtual_ with
      | Some _ -> Ok vlib
      | None ->
        Error (Error (Error.Not_virtual_lib
                        { impl = info ; loc ; not_vlib = vlib.info })))
  in
  let default_implementation =
    Option.map info.default_implementation ~f:(fun l -> lazy (resolve l)) in
  let implementations =
    Option.map info.virtual_ ~f:(fun _ -> lazy (
      let available_implementations = db.find_implementations name in
      Variant.Map.map available_implementations ~f:(
        List.map ~f:(fun (impl : Lib_info.t) ->
          resolve (impl.loc, impl.name)))))
  in
  let requires, pps, resolved_selects =
    resolve_user_deps db info.requires ~allow_private_deps ~pps:info.pps ~stack
  in
  let requires =
    match implements with
    | None -> requires
    | Some impl ->
      let* impl = impl in
      let+ requires = requires in
      impl :: requires
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
  let t =
    { info
    ; name
    ; unique_id         = id
    ; requires
    ; ppx_runtime_deps
    ; pps
    ; resolved_selects
    ; user_written_deps = Lib_info.user_written_deps info
    ; sub_systems       = Sub_system_name.Map.empty
    ; implements
    ; default_implementation
    ; implementations
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
      St_hidden (t, { name; path = t.info.src_dir; reason })
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

and find_internal db (name : Lib_name.t) ~stack : status =
  match Hashtbl.find db.table name with
  | Some x -> x
  | None   -> resolve_name db name ~stack

and resolve_dep db (name : Lib_name.t) ~allow_private_deps
      ~loc ~stack : t Or_exn.t =
  match find_internal db name ~stack with
  | St_initializing id ->
    Error (Dep_stack.dependency_cycle stack id)
  | St_found lib -> check_private_deps lib ~loc ~allow_private_deps
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

and available_internal db (name : Lib_name.t) ~stack =
  resolve_dep db name ~allow_private_deps:true ~loc:Loc.none ~stack
  |> Result.is_ok

and resolve_simple_deps db names ~allow_private_deps ~stack =
  Result.List.map names ~f:(fun (loc, name) ->
    resolve_dep db name ~allow_private_deps ~loc ~stack)

and resolve_complex_deps db deps ~allow_private_deps ~stack =
  let res, resolved_selects =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_selects) dep ->
      let res, acc_selects =
        match (dep : Dune_file.Lib_dep.t) with
        | Direct (loc, name) ->
          let res =
            resolve_dep db name ~allow_private_deps ~loc ~stack
            >>| List.singleton
          in
          (res, acc_selects)
        | Select { result_fn; choices; loc } ->
          let res, src_fn =
            match
              List.find_map choices ~f:(fun { required; forbidden; file } ->
                if Lib_name.Set.exists forbidden
                     ~f:(available_internal db ~stack) then
                  None
                else
                  match
                    let deps =
                      Lib_name.Set.fold required ~init:[] ~f:(fun x acc ->
                        (loc, x) :: acc)
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
  (* Compute transitive closure *)
  let libs, selects = match (deps : Lib_info.Deps.t) with
    | Simple  names ->
      (resolve_simple_deps db names ~allow_private_deps ~stack, [])
    | Complex names ->
      resolve_complex_deps db names ~allow_private_deps ~stack
  in
  (* Find implementations for virtual libraries. *)
  libs, selects


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
        let* pps = resolve_simple_deps db pps ~allow_private_deps:true ~stack in
        closure_with_overlap_checks None pps ~stack ~linking:true
          ~variants:None
      in
      let deps =
        let* init = deps in
        pps >>=
        Result.List.fold_left ~init ~f:(fun init pp ->
          pp.ppx_runtime_deps >>=
          Result.List.fold_left ~init ~f:(fun acc rt ->
            let+ rt = check_private_deps rt ~loc ~allow_private_deps in
            rt :: acc))
      in
      (deps, pps)
  in
  (deps, pps, resolved_selects)

(* Compute transitive closure of libraries to figure which ones will trigger
   their default implementation.

   Assertion: libraries is a list of virtual libraries with no implementation.
   The goal is to find which libraries can safely be defaulted. *)
and resolve_default_libraries libraries ~variants =
  (* Map from a vlib to vlibs that are implemented in the transitive closure of
     its default impl. *)
  let vlib_status = Vlib_visit.create () in
  (* Reverse map *)
  let vlib_default_parent = ref Map.empty in
  let avoid_direct_parent vlib (impl : lib) =
    match impl.implements with
    | None -> Ok true
    | Some x -> let+ x = x in x <> vlib
  in
  (* Either by variants or by default. *)
  let impl_for vlib =
    find_implementation_for vlib ~variants >>= function
    | Some impl -> Ok (Some impl)
    | None ->
      begin match vlib.default_implementation with
      | None -> Ok None
      | Some d -> Result.map ~f:Option.some (Lazy.force d)
      end
  in
  let impl_different_from_vlib_default vlib (impl : lib) =
    impl_for vlib >>| function
    | None -> true
    | Some lib -> lib <> impl
  in
  let library_is_default lib =
    match Map.find !vlib_default_parent lib with
    | Some (_ :: _) -> None
    | None
    | Some [] ->
      Option.bind lib.default_implementation ~f:(fun lib ->
        Result.to_option (Lazy.force lib))
  in
  (* Gather vlibs that are transitively implemented by another
     vlib's default implementation. *)
  let rec visit ~stack ancestor_vlib =
    Vlib_visit.visit vlib_status ~stack ~f:(fun lib ->
      (* Visit direct dependencies *)
      let* deps = lib.requires in
      let* () =
        List.filter deps ~f:(fun x ->
          match avoid_direct_parent x lib with
          | Ok x -> x
          | Error _ -> false)
        |> Result.List.iter ~f:(visit ~stack:(lib.info :: stack) ancestor_vlib)
      in
      (* If the library is an implementation of some virtual library that
         overrides default, add a link in the graph. *)
      let* () =
        Result.Option.iter lib.implements ~f:(fun vlib ->
          let* res = impl_different_from_vlib_default vlib lib in
          match res, ancestor_vlib with
          | true, None ->
            (* Recursion: no ancestor, vlib is explored *)
            visit ~stack:(lib.info::stack) None vlib
          | true, Some ancestor ->
            vlib_default_parent := Map.Multi.cons
                                     !vlib_default_parent
                                     lib
                                     ancestor;
            visit ~stack:(lib.info :: stack) None vlib
          | false, _ ->
            (* If lib is the default implementation, we'll manage it when
               handling virtual lib. *)
            Ok ())
      in
      (* If the library has an implementation according to variants or default
         impl. *)
      let* impl = impl_for lib in
      begin match impl with
      | None -> Ok ()
      | Some impl -> visit ~stack:(lib.info :: stack) (Some lib) impl
      end
    )
  in
  (* For each virtual library we know which vlibs will be implemented when
     enabling its default implementation. *)
  let+ () = Result.List.iter ~f:(visit ~stack:[] None) libraries in
  List.filter_map ~f:library_is_default libraries

and closure_with_overlap_checks db ts ~stack:orig_stack ~linking ~variants =
  let visited = ref Map.empty in
  let unimplemented = ref Vlib.Unimplemented.empty in
  let res = ref [] in
  let rec loop t ~stack =
    match Map.find !visited t with
    | Some (t', stack') ->
      if t = t' then
        Ok ()
      else
        let req_by = Dep_stack.to_required_by ~stop_at:orig_stack in
        Error
          (Error (Conflict { lib1 = (t'.info, req_by stack')
                           ; lib2 = (t.info, req_by stack )
                           }))
    | None ->
      visited := Map.add !visited t (t, stack);
      let* () =
        match db with
        | None -> Ok ()
        | Some db ->
          match find_internal db t.name ~stack with
          | St_found t' ->
            if t = t' then
              Ok ()
            else begin
              let req_by = Dep_stack.to_required_by stack ~stop_at:orig_stack in
              Error
                (Error (Overlap
                          { in_workspace = t'.info
                          ; installed    = (t.info, req_by)
                          }))
            end
          | _ -> assert false
      in
      let* new_stack = Dep_stack.push stack (to_id t) in
      let* deps = t.requires in
      let* unimplemented' = Vlib.Unimplemented.add !unimplemented t in
      unimplemented := unimplemented';
      let+ () = Result.List.iter deps ~f:(loop ~stack:new_stack) in
      res := (t, stack) :: !res
  in
  (* Closure loop with virtual libraries/variants selection*)
  let rec handle ts ~stack =
    let* () = Result.List.iter ts ~f:(loop ~stack) in
    if not linking then
      Ok ()
    else begin
      (* Virtual libraries: find implementations according to variants. *)
      let* (lst, with_default_impl) =
        !unimplemented
        |> Vlib.Unimplemented.fold ~init:([], []) ~f:(fun lib (lst, def) ->
          let* impl =
            find_implementation_for lib ~variants in
          match impl, lib.default_implementation with
          | None, Some _ ->
            Ok (lst, (lib :: def))
          | None, None ->
            Ok (lst, def)
          | Some (impl : lib), _ ->
            Ok (impl :: lst, def))
      in
      (* Manage unimplemented libraries that have a default implementation. *)
      match lst, with_default_impl with
      | [], [] ->
        Ok ()
      | [], def ->
        resolve_default_libraries def ~variants
        >>= handle ~stack
      | lst, _ ->
        handle lst ~stack
    end
  in
  let* () = handle ts ~stack:orig_stack in
  Vlib.associate (List.rev !res) ~linking ~orig_stack

let closure_with_overlap_checks db l ~variants =
  closure_with_overlap_checks db l ~stack:Dep_stack.empty ~variants

let closure l = closure_with_overlap_checks None l ~variants:None

let requires_exn         t = Result.ok_exn t.requires
let ppx_runtime_deps_exn t = Result.ok_exn t.ppx_runtime_deps
let closure_exn          l ~linking = Result.ok_exn (closure l ~linking)

module Compile = struct
  module Resolved_select = Resolved_select

  type nonrec t =
    { direct_requires   : t list Or_exn.t
    ; requires_link     : t list Or_exn.t Lazy.t
    ; pps               : t list Or_exn.t
    ; resolved_selects  : Resolved_select.t list
    ; lib_deps_info     : Lib_deps_info.t
    ; sub_systems       : Sub_system0.Instance.t Lazy.t Sub_system_name.Map.t
    }

  let make_lib_deps_info ~user_written_deps ~pps ~kind =
    Lib_deps_info.merge
      (Dune_file.Lib_deps.info user_written_deps ~kind)
      (List.map pps ~f:(fun (_, pp) -> (pp, kind))
       |> Lib_name.Map.of_list_reduce ~f:Lib_deps_info.Kind.merge)

  let for_lib db (t : lib) =
    let lib_deps_info =
      make_lib_deps_info
        ~user_written_deps:(Lib_info.user_written_deps t.info)
        ~pps:t.info.pps
        ~kind:(Lib_deps_info.Kind.of_optional t.info.optional)
    in
    let requires_link = lazy (
      t.requires >>= closure_with_overlap_checks
                       db
                       ~linking:false
                       ~variants:None
    ) in
    { direct_requires   = t.requires
    ; requires_link
    ; resolved_selects  = t.resolved_selects
    ; pps               = t.pps
    ; lib_deps_info
    ; sub_systems       = t.sub_systems
    }

  let direct_requires   t = t.direct_requires
  let requires_link     t = t.requires_link
  let resolved_selects  t = t.resolved_selects
  let pps               t = t.pps
  let lib_deps_info     t = t.lib_deps_info
  let sub_systems t =
    Sub_system_name.Map.values t.sub_systems
    |> List.map ~f:(fun (lazy (Sub_system0.Instance.T ((module M), t))) ->
      M.T t)
end

(* Databases *)

module DB = struct
  module Resolve_result = struct
    type t = resolve_result =
      | Not_found
      | Found    of Lib_info.t
      | Hidden   of Lib_info.t * string
      | Redirect of db option * Lib_name.t
  end

  type t = db

  let create ?parent ~resolve ~find_implementations ~all () =
    { parent
    ; resolve
    ; find_implementations
    ; table  = Hashtbl.create 1024
    ; all    = Lazy.from_fun all
    }

  let create_variant_map lib_info_list =
    List.concat_map lib_info_list ~f:(fun (info : Lib_info.t) ->
      match info.implements, info.variant with
      | Some (_, virtual_lib), Some variant -> [virtual_lib, (variant, [info])]
      | _, _ -> [])
    |> List.map ~f:(fun (virtual_lib, content) ->
      (virtual_lib, Variant.Map.of_list_exn [content]))
    |> Lib_name.Map.of_list_reduce ~f:Variant.Map.Multi.rev_union

  (* implementations tagged with a variant are only variant when the they
     implement a virtual library from the same project. *)
  let check_valid_implementations (libmap : resolve_result Lib_name.Map.t) =
    Lib_name.Map.iter libmap ~f:(function
      | Found (lib : Lib_info.t) ->
        begin match lib.implements, lib.variant with
        | Some (loc, implements), Some variant ->
          if not (Lib_name.Map.mem libmap implements) then
            Errors.fail loc
              "Library implementation %a for variant %a implements a library \
               outside the project. This is forbidden."
              Lib_name.pp implements Variant.pp variant
        | _, _ -> ()
        end
      | Redirect (_, _) (* skip b/c [Found] covers *) -> ()
      | Hidden (_, _) -> assert false
      | Not_found -> assert false)

  let create_from_library_stanzas ?parent ~lib_config stanzas =
    let variant_map =
      List.map stanzas ~f:(fun (dir, (conf : Dune_file.Library.t)) ->
        Lib_info.of_library_stanza ~dir ~lib_config conf)
      |> create_variant_map
    in
    let map =
      List.concat_map stanzas ~f:(fun (dir, (conf : Dune_file.Library.t)) ->
        let info = Lib_info.of_library_stanza ~dir ~lib_config conf in
        match conf.public with
        | None ->
          [Dune_file.Library.best_name conf, Resolve_result.Found info]
        | Some p ->
          let name = Dune_file.Public_lib.name p in
          if Lib_name.equal name (Lib_name.of_local conf.name) then
            [name, Found info]
          else
            [ name                       , Found info
            ; Lib_name.of_local conf.name, Redirect (None, name)
            ])
      |> Lib_name.Map.of_list
      |> function
      | Ok x -> x
      | Error (name, _, _) ->
        match
          List.filter_map stanzas ~f:(fun (_, (conf : Dune_file.Library.t)) ->
            if Lib_name.equal name (Lib_name.of_local conf.name) ||
               match conf.public with
               | None -> false
               | Some p -> Lib_name.equal name (Dune_file.Public_lib.name p)
            then Some conf.buildable.loc
            else None)
        with
        | [] | [_] -> assert false
        | loc1 :: loc2 :: _ ->
          die "Library %a is defined twice:\n\
               - %s\n\
               - %s"
            Lib_name.pp_quoted name
            (Loc.to_file_colon_line loc1)
            (Loc.to_file_colon_line loc2)
    in
    check_valid_implementations map;
    create () ?parent
      ~resolve:(fun name ->
        Lib_name.Map.find map name
        |> Option.value ~default:Not_found)
      ~find_implementations:(fun virt ->
        Lib_name.Map.find variant_map virt
        |> Option.value ~default:Variant.Map.empty)
      ~all:(fun () -> Lib_name.Map.keys map)

  let create_from_findlib ?(external_lib_deps_mode=false) findlib =
    let variant_map = lazy (
      Findlib.all_packages findlib
      |> List.map ~f:Lib_info.of_dune_lib
      |> create_variant_map
    ) in
    create ()
      ~resolve:(fun name ->
        match Findlib.find findlib name with
        | Ok pkg -> Found (Lib_info.of_dune_lib pkg)
        | Error e ->
          match e with
          | Not_found ->
            if external_lib_deps_mode then
              let pkg = Findlib.dummy_package findlib ~name in
              Found (Lib_info.of_dune_lib pkg)
            else
              Not_found
          | Hidden pkg ->
            Hidden (Lib_info.of_dune_lib pkg, "unsatisfied 'exist_if'"))
      ~find_implementations:(fun virt ->
        Lib_name.Map.find (Lazy.force variant_map) virt
        |> Option.value ~default:Variant.Map.empty)
      ~all:(fun () ->
        Findlib.all_packages findlib
        |> List.map ~f:Dune_package.Lib.name)

  let find = find
  let find_even_when_hidden = find_even_when_hidden

  let find_implementations t = t.find_implementations

  let resolve t (loc, name) =
    match find t name with
    | Ok _ as res -> res
    | Error reason ->
      Error (Error (Library_not_available
                      { loc
                      ; name
                      ; reason
                      }))

  let find_many t ~loc =
    Result.List.map ~f:(fun name -> resolve t (loc, name))

  let available t name = available_internal t name ~stack:Dep_stack.empty

  let get_compile_info t ?(allow_overlaps=false) name =
    match find_even_when_hidden t name with
    | None ->
      Exn.code_error "Lib.DB.get_compile_info got library that doesn't exist"
        [ "name", Lib_name.to_sexp name ]
    | Some lib ->
      let t = Option.some_if (not allow_overlaps) t in
      Compile.for_lib t lib

  let resolve_user_written_deps_for_exes t exes
        ?(allow_overlaps=false) deps ~pps ~variants =
    let lib_deps_info =
      Compile.make_lib_deps_info
        ~user_written_deps:deps
        ~pps
        ~kind:Required
    in
    let res, pps, resolved_selects =
      resolve_user_deps t (Lib_info.Deps.of_lib_deps deps) ~pps
        ~stack:Dep_stack.empty ~allow_private_deps:true
    in
    let requires_link = lazy (
      res
      >>=
      closure_with_overlap_checks (Option.some_if (not allow_overlaps) t)
        ~linking:true
        ~variants
      |> Result.map_error ~f:(fun e ->
        Dep_path.prepend_exn e (Executables exes))
    ) in
    { Compile.
      direct_requires = res
    ; requires_link
    ; pps
    ; resolved_selects
    ; lib_deps_info
    ; sub_systems = Sub_system_name.Map.empty
    }

  let resolve_pps t pps =
    resolve_simple_deps t ~allow_private_deps:true pps ~stack:Dep_stack.empty

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

(* META files *)

module Meta = struct
  let to_names ts =
    List.fold_left ts ~init:Lib_name.Set.empty ~f:(fun acc t ->
      Lib_name.Set.add acc t.name)

  (* For the deprecated method, we need to put all the runtime
     dependencies of the transitive closure.

     We need to do this because [ocamlfind ocamlc -package ppx_foo]
     will not look for the transitive dependencies of [foo], and the
     runtime dependencies might be attached to a dependency of [foo]
     rather than [foo] itself.

     Sigh... *)
  let ppx_runtime_deps_for_deprecated_method t =
    closure_exn [t] ~linking:false
    |> List.concat_map ~f:ppx_runtime_deps_exn
    |> to_names

  let requires         t = to_names (requires_exn         t)
  let ppx_runtime_deps t = to_names (ppx_runtime_deps_exn t)
end

(* Error reporting *)

let report_lib_error ppf (e : Error.t) =
  let dep_path ppf dp =
    match dp with
    | [] -> ()
    | _ ->
      Format.fprintf ppf "@,   %a" Dep_path.Entries.pp dp
  in
  let lib ppf (info : Lib_info.t) =
    Format.fprintf ppf "%a in %a"
      Lib_name.pp_quoted info.name Path.pp info.src_dir
  in
  let lib_and_dep_path ppf (info, dp) =
    Format.fprintf ppf "%a%a" lib info dep_path dp
  in
  match e with
  | Default_implementation_cycle {cycle} ->
    Format.fprintf ppf
      "@{<error>Error@}: Default implementation cycle detected between the \
       following libraries:@\n\
       @[<v>%a@]@\n"
      (Format.pp_print_list (fun ppf (info : Lib_info.t) ->
         Format.fprintf ppf "-> %a"
           Lib_name.pp_quoted info.name))
      cycle
  | Multiple_implementations_for_virtual_lib {lib; loc; given_variants; conflict}  ->
    let print_default_implementation ppf () =
      match lib.default_implementation with
      | None -> Format.fprintf ppf ""
      | Some (_, x) ->
        Format.fprintf ppf "(default implementation %a)" Lib_name.pp x
    in
    let print_variants ppf () =
      if Variant.Set.is_empty given_variants then
        Format.fprintf ppf ""
      else
        Format.fprintf ppf "with variants %a" Variant.Set.pp given_variants
    in
    Format.fprintf ppf
      "%a@{<error>Error@}: Multiple solutions for the implementation@ \
       of %a %a@ %a@,\
       @[<v>%a@]@\n"
      Errors.print loc
      Lib_name.pp lib.name
      print_default_implementation ()
      print_variants ()
      (Format.pp_print_list (fun ppf (lib : Lib_info.t) ->
         Format.fprintf ppf "-> %a (%a)"
           Lib_name.pp lib.name Variant.pp (
           match lib.variant with
           | Some x -> x
           | None -> Variant.make "err")))
      conflict

  | Double_implementation { impl1; impl2; vlib } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: \
       Conflicting implementations for virtual library %a:@,\
       - %a@,\
       - %a@,\
       This cannot work.@]"
      Lib_name.pp_quoted vlib
      lib_and_dep_path impl1
      lib_and_dep_path impl2
  | No_implementation { for_vlib = (info, dp) } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: \
       No implementation found for virtual library %a (%a).@,%a@]"
      Lib_name.pp_quoted info.name Path.pp info.src_dir
      dep_path dp
  | Library_not_available { loc = _; name; reason } ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %a %a.@\n"
      Lib_name.pp_quoted name
      Error.Library_not_available.Reason.pp reason
  | Conflict { lib1; lib2 } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: Conflict between the following libraries:@,\
       - %a@,\
       - %a@,\
       This cannot work.@]@\n"
      lib_and_dep_path lib1
      lib_and_dep_path lib2
  | Overlap { in_workspace; installed } ->
    Format.fprintf ppf
      "@[<v>@{<error>Error@}: Conflict between the following libraries:@,\
       - %a@,\
       - %a@,\
       This is not allowed.@]@\n"
      lib in_workspace
      lib_and_dep_path installed
  | No_solution_found_for_select { loc } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: No solution found for this select form.@\n"
      Errors.print loc
  | Dependency_cycle cycle ->
    Format.fprintf ppf
      "@{<error>Error@}: Dependency cycle detected between the \
       following libraries:@\n\
       @[<v>%a@]@\n"
      (Format.pp_print_list (fun ppf (path, name) ->
         Format.fprintf ppf "-> %a in %a"
           Lib_name.pp_quoted name Path.pp path))
      cycle
  | Private_deps_not_allowed t ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %a is private, it cannot be a dependency of \
       a public library.\nYou need to give %a a public name.\n"
      Lib_name.pp_quoted t.private_dep.name
      Lib_name.pp_quoted t.private_dep.name
  | Not_virtual_lib { impl ; loc ; not_vlib } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: Library %a is not virtual. \
       It cannot be implemented by %a.\n"
      Errors.print loc
      Lib_name.pp_quoted not_vlib.name
      Lib_name.pp_quoted impl.name
let () =
  Printexc.register_printer
    (function
      | Error e ->
        Some (Format.asprintf "%a" report_lib_error e)
      | _ -> None)

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
             let cmdline =
               List.map l ~f:quote_for_shell |> String.concat ~sep:" "
             in
             Some ("try: " ^ cmdline))
        | Private_deps_not_allowed t ->
          (Some t.loc, None)
        | _ -> (None, None)
      in
      let pp ppf = report_lib_error ppf e in
      Some (Report_error.make_printer ?loc ?hint pp)
    | _ -> None)

let to_dune_lib ({ name ; info ; _ } as lib) ~lib_modules ~foreign_objects ~dir =
  let add_loc = List.map ~f:(fun x -> (info.loc, x.name)) in
  let virtual_ = Option.is_some info.virtual_ in
  let obj_dir = Obj_dir.convert_to_external ~dir (obj_dir lib) in
  let lib_modules =
    Lib_modules.version_installed ~install_dir:obj_dir lib_modules in
  let orig_src_dir =
    if !Clflags.store_orig_src_dir
    then Some (
      match info.orig_src_dir with
      | Some src_dir -> src_dir
      | None ->
        match Path.drop_build_context info.src_dir with
        | None -> info.src_dir
        | Some src_dir -> Path.(of_string (to_absolute_filename src_dir))
    )
    else None
  in
  let foreign_objects =
    match info.foreign_objects with
    | External f -> f
    | Local -> foreign_objects
  in
  Dune_package.Lib.make
    ~obj_dir
    ~orig_src_dir
    ~name
    ~loc:info.loc
    ~kind:info.kind
    ~synopsis:info.synopsis
    ~version:info.version
    ~archives:info.archives
    ~plugins:info.plugins
    ~foreign_archives:info.foreign_archives
    ~foreign_objects
    ~jsoo_runtime:info.jsoo_runtime
    ~requires:(add_loc (requires_exn lib))
    ~ppx_runtime_deps:(add_loc (ppx_runtime_deps_exn lib))
    ~modes:info.modes
    ~implements:info.implements
    ~variant:info.variant
    ~default_implementation:info.default_implementation
    ~virtual_
    ~modules:(Some lib_modules)
    ~main_module_name:(Result.ok_exn (main_module_name lib))
    ~sub_systems:(Sub_system.dump_config lib)


