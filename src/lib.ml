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
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list

    let of_lib_deps deps =
      let rec loop acc (deps : Jbuild.Lib_dep.t list) =
        match deps with
        | []                  -> Some (List.rev acc)
        | Direct name :: deps -> loop (name :: acc) deps
        | Select _    :: _    -> None
      in
      match loop [] deps with
      | Some l -> Simple l
      | None   -> Complex deps
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
    ; ppx_runtime_deps : string list
    ; pps              : Jbuild.Pp.t list
    ; optional         : bool
    }

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
    ; requires         = Deps.of_lib_deps conf.buildable.libraries
    ; ppx_runtime_deps = conf.ppx_runtime_libraries
    ; pps = Jbuild.Preprocess_map.pps conf.buildable.preprocess
    }

  let of_findlib_package pkg =
    let module P = Findlib.Package in
    { loc              = Loc.in_file (Path.to_string (P.meta_file pkg))
    ; kind             = Normal
    ; src_dir          = P.dir pkg
    ; obj_dir          = P.dir pkg
    ; version          = P.version pkg
    ; synopsis         = P.description pkg
    ; archives         = P.archives pkg
    ; plugins          = P.plugins pkg
    ; jsoo_runtime     = P.jsoo_runtime pkg
    ; requires         = Simple (P.requires pkg)
    ; ppx_runtime_deps = P.ppx_runtime_deps pkg
    ; pps              = []
    ; optional         = false
    ; status           = Installed
    ; (* We don't know how these are named for external libraries *)
      foreign_archives = Mode.Dict.make_both []
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
      { name   : string
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

module Init = struct
  type t =
    { unique_id : int
    ; path      : Path.t
    ; name      : string
    }
end

type t =
  { loc              : Loc.t
  ; name             : string
  ; unique_id        : int
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
  ; requires         : t list or_error
  ; ppx_runtime_deps : t list or_error
  ; resolved_selects : Resolved_select.t list
  }

and db =
  { parent  : db option
  ; resolve : string -> (info_or_redirect,
                         Error0.Library_not_available.Reason.t) result
  ; table   : (string, resolve_status) Hashtbl.t
  ; all     : string list Lazy.t
  }

and resolve_status =
  | Initializing of Init.t
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
  { lib1 : t * With_required_by.Entry.t list
  ; lib2 : t * With_required_by.Entry.t list
  }

and 'a or_error = ('a, error With_required_by.t) result

module Error = struct
  include Error0

  module Conflict = struct
    type nonrec t = conflict =
      { lib1 : t * With_required_by.Entry.t list
      ; lib2 : t * With_required_by.Entry.t list
      }
  end

  type t = error =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t
end

exception Error of Error.t With_required_by.t

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

let to_init t : Init.t =
  { unique_id = t.unique_id
  ; path      = t.src_dir
  ; name      = t.name
  }

module L = struct
  type nonrec t = t list

  let include_paths ts ~stdlib_dir =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      Path.Set.add (obj_dir t) acc)
    |> Path.Set.remove stdlib_dir

  let include_flags ts ~stdlib_dir =
    let dirs = include_paths ts ~stdlib_dir in
    Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
      [Arg_spec.A "-I"; Path dir]))

  let c_include_flags ts ~stdlib_dir =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        Path.Set.add t.src_dir acc)
      |> Path.Set.remove stdlib_dir
    in
    Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
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
        if Int_set.mem x.unique_id seen then
          loop acc l seen
        else
          loop (x :: acc) l (Int_set.add x.unique_id seen)
    in
    loop [] l Int_set.empty
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
    { stack : Init.t list
    ; seen  : Int_set.t
    }

  let empty =
    { stack = []
    ; seen  = Int_set.empty
    }

  let dependency_cycle t (last : Init.t) =
    assert (Int_set.mem last.unique_id t.seen);
    let rec build_loop acc stack =
      match stack with
      | [] -> assert false
      | (x : Init.t) :: stack ->
        let acc = (x.path, x.name) :: acc in
        if x.unique_id = last.unique_id then
          acc
        else
          build_loop acc stack
    in
    let loop = build_loop [(last.path, last.name)] t.stack in
    { With_required_by.
      data        = Dependency_cycle loop
    ; required_by = []
    }

  let create_and_push t name path =
    let unique_id = gen_unique_id () in
    let init = { Init. unique_id; name; path } in
    (init,
     { stack = init :: t.stack
     ; seen  = Int_set.add unique_id t.seen
     })

  let push t (x : Init.t) : (_, _) result =
    if Int_set.mem x.unique_id t.seen then
      Error (dependency_cycle t x)
    else
      Ok { stack = x :: t.stack
         ; seen  = Int_set.add x.unique_id t.seen
         }
end

let map_find_result name res : (_, _) result =
  match res with
  | Ok _ as res -> res
  | Error reason ->
    Error { With_required_by.
            data        = Error.Library_not_available { name; reason }
          ; required_by = []
          }

let rec make db name (info : Info.t) ~unique_id ~stack =
  let requires, resolved_selects =
    resolve_user_deps db info.requires ~pps:info.pps ~stack
  in
  let ppx_runtime_deps =
    resolve_simple_deps db info.ppx_runtime_deps ~stack
  in
  let map_error x =
    Result.map_error x ~f:(fun e ->
      With_required_by.prepend_one e (Library (info.src_dir, name)))
  in
  let requires         = map_error requires         in
  let ppx_runtime_deps = map_error ppx_runtime_deps in
  { loc              = info.loc
  ; name             = name
  ; unique_id        = unique_id
  ; kind             = info.kind
  ; status           = info.status
  ; src_dir          = info.src_dir
  ; obj_dir          = info.obj_dir
  ; version          = info.version
  ; synopsis         = info.synopsis
  ; archives         = info.archives
  ; plugins          = info.plugins
  ; foreign_archives = info.foreign_archives
  ; jsoo_runtime     = info.jsoo_runtime
  ; requires         = requires
  ; ppx_runtime_deps = ppx_runtime_deps
  ; resolved_selects = resolved_selects
  }

and find db name =
  match Hashtbl.find db.table name with
  | Some (Initializing _) -> assert false
  | Some (Done x) -> x
  | None -> resolve_name db name ~stack:Dep_stack.empty

and find_internal db name ~stack : (_, _) result =
  match Hashtbl.find db.table name with
  | Some (Initializing init) ->
    Error (Dep_stack.dependency_cycle stack init)
  | Some (Done x) -> map_find_result name x
  | None -> map_find_result name (resolve_name db name ~stack)

and resolve_name db name ~stack =
  match db.resolve name with
  | Ok (Proxy t) ->
    let res = Ok t in
    Hashtbl.replace db.table ~key:name ~data:(Done res);
    res
  | Ok (Redirect (_loc, path, name')) ->
    let init, stack =
      Dep_stack.create_and_push stack name path
    in
    Hashtbl.add db.table ~key:name ~data:(Initializing init);
    let res =
      match find_internal db name' ~stack with
      | Ok _ as res -> res
      | Error _ ->
        match Hashtbl.find db.table name' with
        | Some (Done res) -> res
        | _ -> assert false
    in
    Hashtbl.replace db.table ~key:name ~data:(Done res);
    res
  | Ok (Info info) ->
    let init, stack =
      Dep_stack.create_and_push stack name info.src_dir
    in
    (* Add [init] to the table, to detect loops *)
    Option.iter (Hashtbl.find db.table name) ~f:(fun x ->
      let to_sexp = Sexp.To_sexp.(pair Path.sexp_of_t atom) in
      let sexp =
        match x with
        | Initializing x ->
          Sexp.List [Atom "Initializing"; Path.sexp_of_t x.path]
        | Done (Ok t) -> List [Atom "Ok"; Path.sexp_of_t t.src_dir]
        | Done (Error Not_found) -> Atom "Not_found"
        | Done (Error (Hidden { info; reason; _ })) ->
          List [Atom "Hidden"; Path.sexp_of_t info.src_dir; Atom reason]
      in
        Sexp.code_error
          "Lib_db.DB: resolver returned name that's already in the table"
          [ "name"            , Atom name
          ; "returned_lib"    , to_sexp (info.src_dir, name)
          ; "conflicting_with", sexp
          ]);
    Hashtbl.add db.table ~key:name ~data:(Initializing init);
    let t = make db name info ~unique_id:init.unique_id ~stack in
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
    Hashtbl.add db.table ~key:name ~data:(Done res);
    res

and available_internal db name ~stack =
  match find_internal db name ~stack with
  | Ok    _ -> true
  | Error _ -> false

and resolve_simple_deps db names ~stack =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | name :: names ->
      find_internal db name ~stack >>= fun x ->
      loop (x :: acc) names
  in
  loop [] names

and resolve_complex_deps db deps ~stack =
  let res, resolved_selects =
    List.fold_left deps ~init:(Ok [], []) ~f:(fun (acc_res, acc_selects) dep ->
      let res, acc_selects =
        match (dep : Jbuild.Lib_dep.t) with
        | Direct name ->
          let res =
            find_internal db name ~stack >>| fun x -> [x]
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
                    resolve_simple_deps db (String_set.elements required) ~stack
                  with
                  | Ok ts -> Some (ts, file)
                  | Error _ -> None)
            with
            | Some (ts, file) ->
              (Ok ts, Ok file)
            | None ->
              let e = { Error.No_solution_found_for_select.loc } in
              (Error { With_required_by.
                       data        = No_solution_found_for_select e
                     ; required_by = []
                     },
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
  let deps =
    match pps with
    | [] -> deps
    | pps ->
      let pps = List.map pps ~f:Jbuild.Pp.to_string in
      deps >>= fun deps ->
      resolve_simple_deps db pps ~stack >>= fun pps ->
      fold_closure pps ~stack ~init:deps ~f:(fun t acc ->
        t.ppx_runtime_deps >>= fun rt_deps ->
        Ok (List.rev_append rt_deps acc))
  in
  (deps, resolved_selects)

(* Fold the transitive closure in arbitrary order *)
and fold_closure ts ~init ~f ~stack =
  let seen = ref Int_set.empty in
  let rec loop ts acc ~stack =
    match ts with
    | [] -> Ok acc
    | t :: ts ->
      if Int_set.mem t.unique_id !seen then
        loop ts acc ~stack
      else begin
        seen := Int_set.add t.unique_id !seen;
        f t acc >>= fun acc ->
        (Dep_stack.push stack (to_init t) >>= fun stack ->
         t.requires >>= fun deps ->
         loop deps acc ~stack)
        >>= fun acc ->
        loop ts acc ~stack
      end
  in
  loop ts init ~stack

let to_exn res ~required_by =
  match res with
  | Ok    x -> x
  | Error e -> raise (Error (With_required_by.append e required_by))

let requires_exn t ~required_by =
  to_exn t.requires ~required_by
let ppx_runtime_deps_exn t ~required_by =
  to_exn t.ppx_runtime_deps ~required_by

(* +-----------------------------------------------------------------+
   | Transitive closure                                              |
   +-----------------------------------------------------------------+ *)

module Closure =
  Top_closure.Make
    (String)
    (struct
      type graph = unit
      type nonrec t = t * With_required_by.Entry.t list
      let key (t, _) = t.name
      let deps (t, required_by) () =
        let required_by =
          With_required_by.Entry.Library (t.src_dir, t.name) :: required_by
        in
        List.map (requires_exn t ~required_by) ~f:(fun x -> (x, required_by))
    end)

exception Conflict_found of Error.Conflict.t

let check_conflicts ts =
  match
    List.fold_left ts ~init:String_map.empty ~f:(fun acc t ->
      let name = (fst t).name in
      match String_map.find name acc with
      | None -> String_map.add acc ~key:name ~data:t
      | Some t' -> raise_notrace (Conflict_found { lib1 = t'; lib2 = t }))
  with
  | (_ : _ String_map.t) ->
    Ok (List.map ts ~f:fst)
  | exception (Conflict_found c) ->
    Error { With_required_by.
            data        = Conflict c
          ; required_by = []
          }

let closure_cache = Hashtbl.create 1024

let closure ts =
  match ts with
  | [] -> Ok []
  | _ ->
    let key = List.map ts ~f:(fun p -> p.unique_id) in
    Hashtbl.find_or_add closure_cache key ~f:(fun _ ->
      let ts = List.map ts ~f:(fun t -> (t, [])) in
      match Closure.top_closure () ts with
      | Ok ts -> check_conflicts ts
      | Error cycle ->
        let required_by = snd (List.hd cycle) in
        let cycle =
          List.map cycle ~f:(fun (t, _) -> t.src_dir, t.name)
        in
        Error { With_required_by.
                data = Dependency_cycle cycle
              ; required_by
              }
      | exception (Error e) -> Error e)

let closure_exn ts ~required_by = to_exn (closure ts) ~required_by

module Compile = struct
  module Resolved_select = Resolved_select

  let requires t = t.requires >>= closure

  let resolved_selects t = t.resolved_selects
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
      |> String_map.of_alist
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
        match String_map.find name map with
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

  let find_exn t name ~required_by =
    match find t name with
    | Ok x -> x
    | Error reason ->
      raise (Error { data = Library_not_available { name; reason }
                   ; required_by
                   })

  let available t name = available_internal t name ~stack:Dep_stack.empty

  let resolve_user_written_deps t deps ~pps =
    let res, resolved_select =
      resolve_user_deps t (Info.Deps.of_lib_deps deps) ~pps
        ~stack:Dep_stack.empty
    in
    let res = res >>= closure in
    (res, resolved_select)

  let resolve_pps t pps =
    resolve_simple_deps t (List.map pps ~f:Jbuild.Pp.to_string)
      ~stack:Dep_stack.empty

  let rec all ?(recursive=false) t =
    let l =
      List.filter_map (Lazy.force t.all) ~f:(fun name ->
        match find t name with
        | Ok x -> Some x
        | Error (Hidden _) -> None
        | Error reason ->
          raise (Error { data        = Library_not_available { name; reason }
                       ; required_by = []
                       }))
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
      String_set.add t.name acc)

  (* For the deprecated method, we need to put all the runtime
     dependencies of the transitive closure.

     We need to do this because [ocamlfind ocamlc -package ppx_foo]
     will not look for the transitive dependencies of [foo], and the
     runtime dependencies might be attached to a dependency of [foo]
     rather than [foo] itself.

     Sigh... *)
  let ppx_runtime_deps_for_deprecated_method t ~required_by =
    closure_exn [t] ~required_by
    |> List.concat_map ~f:(ppx_runtime_deps_exn ~required_by)
    |> to_names

  let requires t ~required_by =
    to_names (requires_exn t ~required_by)
  let ppx_runtime_deps t ~required_by =
    to_names (ppx_runtime_deps_exn t ~required_by)
end

(* +-----------------------------------------------------------------+
   | Error reporting                                                 |
   +-----------------------------------------------------------------+ *)


let report_lib_error ppf (e : Error.t) ~required_by =
  match e with
  | Library_not_available { name; reason } ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %S %a.@\n%a@\n"
      name
      Error.Library_not_available.Reason.pp reason
      With_required_by.Entries.pp required_by;
    (match !Clflags.external_lib_deps_hint with
     | [] -> (* during bootstrap *) ()
     | l ->
       Format.fprintf ppf
         "Hint: try: %s\n"
         (List.map l ~f:quote_for_shell |> String.concat ~sep:" "))
  | Conflict { lib1 = (lib1, rb1); lib2 = (lib2, rb2) } ->
    Format.fprintf ppf
      "@{<error>Error@}: Conflict between the following libaries:\n\
       - %S in %s\n\
      \    %a\n
       - %S in %s\n\
      \    %a\n
       %a\n
       This cannot work.\n"
      lib1.name (Path.to_string_maybe_quoted lib1.src_dir)
      With_required_by.Entries.pp rb1
      lib2.name (Path.to_string_maybe_quoted lib2.src_dir)
      With_required_by.Entries.pp rb2
      With_required_by.Entries.pp required_by
  | No_solution_found_for_select { loc } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: No solution found for this select form.\n"
      Loc.print loc
  | Dependency_cycle cycle ->
    Format.fprintf ppf
      "@{<error>Error@}: Dependency cycle detected between the \
       following libraries:\n\
       @[<v>%a@]\n\
       Required by:\n\
       %a\n"
      (Format.pp_print_list (fun ppf (path, name) ->
         Format.fprintf ppf "-> %S in %s"
           name (Path.to_string_maybe_quoted path)))
      cycle
      With_required_by.Entries.pp required_by

let break_at_loc ppf required_by =
  let rec loop acc (l : With_required_by.Entry.t list) =
    match l with
    | [] -> None
    | Loc loc :: _ -> Some (loc, List.rev acc)
    | e :: l -> loop (e :: acc) l
  in
  match loop [] required_by with
  | None -> required_by
  | Some (loc, required_by) ->
    Loc.print ppf loc;
    required_by

let () =
  Report_error.register (fun ppf exn ->
    match exn with
    | Error { data = e; required_by } ->
      let required_by = break_at_loc ppf required_by in
      report_lib_error ppf e ~required_by;
      true
    | _ -> false)
