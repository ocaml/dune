open Import
open Memo.O
module Opam_package = Package
module P = Ocaml.Variant
module Ps = Ocaml.Variant.Set
module Findlib = Dune_findlib
module Vars = Dune_findlib.Vars
module Rules = Dune_findlib.Rules

module Unavailable_reason = struct
  type t =
    | Not_found
    | Invalid_dune_package of User_message.t

  let to_dyn =
    let open Dyn in
    function
    | Not_found -> variant "Not_found" []
    | Invalid_dune_package why ->
      variant "Invalid_dune_package" [ Dyn.string (User_message.to_string why) ]
  ;;
end

let builtin_for_dune : Dune_package.t =
  let entry =
    Dune_package.Entry.Deprecated_library_name
      { loc = Loc.of_pos __POS__
      ; old_public_name = Lib_name.of_string "dune.configurator"
      ; new_public_name = Lib_name.of_string "dune-configurator"
      }
  in
  { name = Opam_package.Name.of_string "dune"
  ; entries = Lib_name.Map.singleton (Dune_package.Entry.name entry) entry
  ; version = None
  ; dir = Path.root
  ; sections = Section.Map.empty
  ; sites = Site.Map.empty
  ; files = []
  }
;;

module DB = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; stdlib_dir : Path.t
    ; paths : Path.t list
    ; builtins : Meta.Simplified.t Package.Name.Map.t
    ; ext_lib : Filename.Extension.t
    }

  let to_dyn { id; stdlib_dir; paths; builtins; ext_lib } =
    let open Dyn in
    record
      [ "id", Id.to_dyn id
      ; "stdlib_dir", Path.to_dyn stdlib_dir
      ; "paths", list Path.to_dyn paths
      ; "builtins", Package.Name.Map.to_dyn Meta.Simplified.to_dyn builtins
      ; "ext_lib", string ext_lib
      ]
  ;;

  let equal x y = Id.equal x.id y.id
  let hash t = Id.hash t.id

  let create ~paths ~(lib_config : Lib_config.t) =
    let stdlib_dir = lib_config.stdlib_dir in
    let ext_lib = lib_config.ext_lib in
    let+ builtins =
      let version = lib_config.ocaml_version in
      Meta.builtins ~stdlib_dir ~version
    in
    { id = Id.gen (); stdlib_dir; paths; builtins; ext_lib }
  ;;
end

let resolve_link ~dir ~fname (kind : File_kind.t) =
  match kind with
  | S_LNK ->
    (match Path.Untracked.stat (Path.relative dir fname) with
     | Ok { Unix.st_kind; _ } -> Some st_kind
     | Error _ -> None)
  | _ -> Some kind
;;

let to_dune_library (t : Findlib.Package.t) ~dir_contents ~ext_lib ~external_location =
  let loc = Loc.in_file t.meta_file in
  let add_loc x = loc, x in
  let archives = Findlib.Package.archives t in
  let obj_dir = Obj_dir.make_external_no_private ~dir:t.dir in
  let modes : Lib_mode.Map.Set.t =
    (* libraries without archives are compatible with all modes. mainly a
       hack for compiler-libs which doesn't have any archives *)
    let discovered = Mode.Dict.map ~f:List.is_non_empty archives in
    let modes =
      if Mode.Dict.Set.is_empty discovered then Mode.Dict.Set.all else discovered
    in
    { Lib_mode.Map.ocaml = modes; melange = false }
  in
  let dir_contents =
    Result.map
      dir_contents
      ~f:
        (List.filter_map ~f:(fun (fname, kind) ->
           match resolve_link ~dir:t.dir ~fname kind with
           | Some S_REG -> Some fname
           | _ -> None))
  in
  let (info : Path.t Lib_info.t) =
    let kind = Findlib.Package.kind t in
    let sub_systems = Sub_system_name.Map.empty in
    let synopsis = Findlib.Package.description t in
    let status =
      match Lib_name.analyze t.name with
      | Private (_, _) -> Lib_info.Status.Installed_private
      | Public (_, _) -> Lib_info.Status.Installed
    in
    let src_dir = Obj_dir.dir obj_dir in
    let version =
      (* Errors are silently ignored: versions in META files are less strict
         than what we allow in `Package_version` *)
      Option.bind (Findlib.Package.version t) ~f:Package_version.of_string_opt
    in
    let dune_version = None in
    let virtual_deps = [] in
    let implements = None in
    let parameters = [] in
    let orig_src_dir = None in
    let local_main_module_name = None in
    let main_module_name : Lib_info.Main_module_name.t = This None in
    let enabled = Memo.return Lib_info.Enabled_status.Normal in
    let requires =
      let ocaml =
        let exports = Lib_name.Set.of_list (Findlib.Package.exports t) in
        Findlib.Package.requires t
        |> List.map ~f:(fun name ->
          let lib_dep =
            if Lib_name.Set.mem exports name then Lib_dep.re_export else Lib_dep.direct
          in
          lib_dep (add_loc name))
      in
      { Compilation_mode.By_mode.ocaml; melange = [] }
    in
    let ppx_runtime_deps =
      { Compilation_mode.By_mode.ocaml =
          List.map (Findlib.Package.ppx_runtime_deps t) ~f:add_loc
      ; melange = []
      }
    in
    let special_builtin_support : (Loc.t * Lib_info.Special_builtin_support.t) option =
      (* findlib has been around for much longer than dune, so it is
         acceptable to have a special case in dune for findlib. *)
      match Lib_name.to_string t.name with
      | "findlib.dynload" -> Some (loc, Findlib_dynload)
      | _ -> None
    in
    let foreign_objects = Lib_info.Source.External [] in
    let public_headers = Lib_info.File_deps.External [] in
    let plugins = Findlib.Package.plugins t in
    let jsoo_runtime = Findlib.Package.jsoo_runtime t in
    let wasmoo_runtime = Findlib.Package.wasmoo_runtime t in
    let melange_runtime_deps = Lib_info.File_deps.External [] in
    let preprocess =
      Compilation_mode.By_mode.both (Preprocess.Per_module.no_preprocessing ())
    in
    let default_implementation = None in
    let wrapped = None in
    let foreign_archives, native_archives =
      (* Here we scan [t.dir] and consider all files named [lib*.ext_lib] to
         be foreign archives, and all other files with the extension
         [ext_lib] to be native archives. The resulting lists of archives
         will be used to compute appropriate flags for linking dependent
         executables. *)
      match dir_contents with
      | Error _ ->
        (* Raising an error is not an option here as we systematically delay
           all library loading errors until the libraries are actually used
           in rules.

           We could add a warning like this:

           User_warning.emit ~loc:(Loc.in_dir t.dir) [ Pp.text "Unable to
               read directory" ];

           But it seems to be too invasive *)
        [], []
      | Ok dir_contents ->
        List.rev_filter_partition_map dir_contents ~f:(fun f ->
          let ext = Filename.extension f in
          if ext = ext_lib
          then (
            let file = Path.relative t.dir f in
            if String.is_prefix f ~prefix:Foreign.Archive.Name.lib_file_prefix
            then Left file
            else Right file)
          else Skip)
    in
    let foreign_archives = Mode.Map.Multi.create_for_all_modes foreign_archives in
    let entry_modules =
      Lib_info.Source.External
        (match Vars.get_words t.vars "main_modules" Ps.empty with
         | _ :: _ as modules ->
           Ok
             (Compilation_mode.By_mode.just
                ~for_:Ocaml
                (List.map ~f:Module_name.of_checked_string modules))
         | [] ->
           (match dir_contents with
            | Error (e, _, _) ->
              Error
                (User_message.make
                   [ Pp.textf
                       "Unable to get entry modules of %s in %s. "
                       (Lib_name.to_string t.name)
                       (Path.to_string src_dir)
                   ; Pp.textf "error: %s" (Unix.error_message e)
                   ])
            | Ok dir_contents ->
              let ext = Cm_kind.ext Cmi in
              Result.List.filter_map dir_contents ~f:(fun fname ->
                match Filename.check_suffix fname ext with
                | false -> Ok None
                | true ->
                  if
                    (* We add this hack to skip manually mangled
                        libraries *)
                    String.contains_double_underscore fname
                  then Ok None
                  else (
                    match
                      let name = Filename.remove_extension fname in
                      Module_name.of_string_user_error (Loc.in_dir src_dir, name)
                    with
                    | Ok s -> Ok (Some s)
                    | Error e -> Error e))
              |> Result.map ~f:(Compilation_mode.By_mode.just ~for_:Ocaml)))
    in
    let modules = Lib_info.Source.External (Compilation_mode.By_mode.both None) in
    let name = t.name in
    let lib_id = Lib_id.External (loc, name) in
    Lib_info.create
      ~loc
      ~path_kind:External
      ~name
      ~lib_id
      ~kind
      ~status
      ~src_dir
      ~orig_src_dir
      ~obj_dir
      ~version
      ~synopsis
      ~main_module_name
      ~local_main_module_name
      ~sub_systems
      ~requires
      ~parameters
      ~foreign_objects
      ~public_headers
      ~plugins
      ~archives
      ~ppx_runtime_deps
      ~allow_unused_libraries:[]
      ~foreign_archives
      ~native_archives:(Files native_archives)
      ~foreign_dll_files:[]
      ~jsoo_runtime
      ~wasmoo_runtime
      ~preprocess
      ~enabled
      ~virtual_deps
      ~dune_version
      ~entry_modules
      ~implements
      ~default_implementation
      ~modes
      ~modules
      ~wrapped
      ~special_builtin_support
      ~exit_module:None
      ~instrumentation_backend:None
      ~melange_runtime_deps
      ~root_module:None
  in
  Dune_package.Lib.of_findlib info external_location
;;

module Loader = struct
  open Memo.O

  module Findlib_dir = struct
    type t =
      { sub_dirs : Filename.Set.t
      ; metas : Filename.Set.t
      }

    let empty = { sub_dirs = Filename.Set.empty; metas = Filename.Set.empty }
    let file_prefix = Findlib.Package.meta_fn ^ "."

    let of_path =
      let impl path =
        Fs.dir_contents path
        >>| function
        | Error e -> Error e
        | Ok contents ->
          let sub_dirs, metas =
            List.filter_partition_map contents ~f:(fun (name, kind) ->
              match resolve_link ~dir:path ~fname:name kind with
              | Some S_DIR -> Left name
              | Some S_REG when String.is_prefix name ~prefix:file_prefix -> Right name
              | _ -> Skip)
          in
          Ok
            { sub_dirs = Filename.Set.of_list sub_dirs
            ; metas = Filename.Set.of_list metas
            }
      in
      let memo = Memo.create "read-findlib-path" ~input:(module Path) impl in
      Memo.exec memo
    ;;

    let of_path_ignore_error path =
      of_path path
      >>| function
      | Error _ -> empty
      | Ok s -> s
    ;;
  end

  (* Parse all the packages defined in a META file *)
  let dune_package_of_meta (db : DB.t) ~loc ~meta_file ~(meta : Meta.Simplified.t) =
    let dir_of_loc (loc : Dune_package.External_location.t) =
      match loc with
      | Absolute d -> d
      | Relative_to_findlib (dir, l) -> Path.relative dir (Path.Local.to_string l)
      | Relative_to_stdlib l -> Path.relative db.stdlib_dir (Path.Local.to_string l)
    in
    let rec loop ~loc ~full_name (meta : Meta.Simplified.t) acc =
      let vars = Vars.of_meta_rules meta.vars in
      let external_location : Dune_package.External_location.t =
        match Vars.get vars "directory" Ps.empty with
        | None | Some "" -> loc
        | Some pkg_dir ->
          if pkg_dir.[0] = '+' || pkg_dir.[0] = '^'
          then Relative_to_stdlib (Path.Local.of_string (String.drop pkg_dir 1))
          else if Filename.is_relative pkg_dir
          then (
            match loc with
            | Relative_to_findlib (cur, sub) ->
              Relative_to_findlib (cur, Path.Local.relative sub pkg_dir)
            | Absolute path -> Absolute (Path.relative path pkg_dir)
            | Relative_to_stdlib sub ->
              Relative_to_stdlib (Path.Local.relative sub pkg_dir))
          else Absolute (Path.of_filename_relative_to_initial_cwd pkg_dir)
      in
      let* (entry : Dune_package.Entry.t) =
        let pkg : Findlib.Package.t =
          { Findlib.Package.meta_file
          ; name = full_name
          ; dir = dir_of_loc external_location
          ; vars
          }
        in
        let* lib =
          let+ dir_contents = Fs.dir_contents pkg.dir in
          to_dune_library pkg ~dir_contents ~ext_lib:db.ext_lib ~external_location
        in
        let+ exists =
          Findlib.Package.exists
            pkg
            ~is_builtin:
              (Package.Name.Map.mem db.builtins (Lib_name.package_name pkg.name))
        in
        if exists then Dune_package.Entry.Library lib else Hidden_library lib
      in
      let acc = Lib_name.Map.add_exn acc (Dune_package.Entry.name entry) entry in
      Memo.List.fold_left meta.subs ~init:acc ~f:(fun acc (meta : Meta.Simplified.t) ->
        let full_name =
          match meta.name with
          | None -> full_name
          | Some name -> Lib_name.nest full_name name
        in
        loop ~loc:external_location ~full_name meta acc)
    in
    let name = Option.value_exn meta.name in
    let+ entries = loop ~loc ~full_name:name meta Lib_name.Map.empty in
    { Dune_package.name = Lib_name.package_name name
    ; version =
        Lib_name.Map.find entries name |> Option.bind ~f:Dune_package.Entry.version
    ; entries
    ; dir = dir_of_loc loc
    ; sections = Section.Map.empty
    ; sites = Site.Map.empty
    ; files = []
    }
  ;;

  let load_builtin db meta =
    dune_package_of_meta
      db
      ~loc:(Relative_to_stdlib (Path.Local.of_string "."))
      ~meta_file:(Path.of_string "<internal>")
      ~meta
  ;;

  let lookup db name findlib_dir
    : (Dune_package.t, Unavailable_reason.t) result option Memo.t
    =
    let load_meta ~findlib_dir ~dir meta_file =
      let* meta = Fs.with_lexbuf_from_file meta_file ~f:(Meta.of_lex ~name:(Some name)) in
      let loc = Dune_package.External_location.Relative_to_findlib (findlib_dir, dir) in
      dune_package_of_meta db ~loc ~meta_file ~meta >>| Option.some
    in
    let* dir_contents = Findlib_dir.of_path_ignore_error findlib_dir in
    (* XXX DUNE4 why do we allow [META.foo] override [dune-package] file? *)
    let meta_fn = Findlib_dir.file_prefix ^ Package.Name.to_string name in
    if Filename.Set.mem dir_contents.metas meta_fn
    then
      Path.relative findlib_dir meta_fn
      |> load_meta ~findlib_dir ~dir:Path.Local.root
      >>| Option.map ~f:Result.ok
    else (
      match Filename.Set.mem dir_contents.sub_dirs (Package.Name.to_string name) with
      | false -> Memo.return None
      | true ->
        let dir = Path.relative findlib_dir (Package.Name.to_string name) in
        let* files =
          Fs.dir_contents dir
          >>| (function
           | Error _ -> []
           | Ok s -> s)
          >>| List.filter_map ~f:(fun (name, (kind : File_kind.t)) ->
            match name = Dune_package.fn || name = Findlib.Package.meta_fn with
            | false -> None
            | true ->
              (match resolve_link ~dir ~fname:name kind with
               | Some S_REG -> Some name
               | _ -> None))
          >>| Filename.Set.of_list
        in
        (if Filename.Set.mem files Dune_package.fn
         then Path.relative dir Dune_package.fn |> Dune_package.Or_meta.load
         else Memo.return (Ok Dune_package.Or_meta.Use_meta))
        >>= (function
         | Error e ->
           Memo.return (Some (Error (Unavailable_reason.Invalid_dune_package e)))
         | Ok (Dune_package.Or_meta.Dune_package p) -> Memo.return (Some (Ok p))
         | Ok Use_meta ->
           (match Filename.Set.mem files Findlib.Package.meta_fn with
            | false -> Memo.return None
            | true ->
              Path.relative dir Findlib.Package.meta_fn
              |> load_meta
                   ~findlib_dir
                   ~dir:(Path.Local.of_string (Package.Name.to_string name))
              >>| Option.map ~f:(fun pkg -> Ok pkg))))
  ;;

  let lookup_and_load (db : DB.t) name =
    Memo.List.find_map db.paths ~f:(lookup db name)
    >>= function
    | Some m -> Memo.return m
    | None ->
      (match Package.Name.to_string name with
       | "dune" -> Memo.return (Ok builtin_for_dune)
       | _ ->
         (match Package.Name.Map.find db.builtins name with
          | None -> Memo.return (Error Unavailable_reason.Not_found)
          | Some meta -> load_builtin db meta >>| Result.ok))
  ;;

  let root_packages (db : DB.t) =
    let+ pkgs =
      Memo.List.concat_map db.paths ~f:(fun dir ->
        Findlib_dir.of_path dir
        >>= function
        | Error (ENOENT, _, _) -> Memo.return []
        | Error (unix_error, _, _) ->
          User_error.raise
            [ Pp.textf
                "Unable to read directory %s for findlib package"
                (Path.to_string_maybe_quoted dir)
            ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
            ]
        | Ok { sub_dirs; metas } ->
          let+ sub_dirs =
            Filename.Set.to_list sub_dirs
            |> Memo.List.filter_map ~f:(fun name ->
              Path.L.relative dir [ name; Findlib.Package.meta_fn ]
              |> Fs.file_exists
              >>| function
              | true -> Some (Package.Name.of_string name)
              | false -> None)
          in
          let metas =
            Filename.Set.to_list_map metas ~f:(fun fn ->
              String.drop_prefix ~prefix:Findlib_dir.file_prefix fn
              |> Option.value_exn
              |> Package.Name.of_string)
          in
          List.rev_append sub_dirs metas)
      >>| Package.Name.Set.of_list
    in
    Package.Name.Set.of_keys db.builtins |> Package.Name.Set.union pkgs
  ;;
end

module Public = struct
  let memo =
    let module Input = struct
      type nonrec t = DB.t * Package.Name.t

      let to_dyn = Tuple.T2.to_dyn DB.to_dyn Package.Name.to_dyn
      let hash = Tuple.T2.hash DB.hash Package.Name.hash
      let equal = Tuple.T2.equal DB.equal Package.Name.equal
    end
    in
    Memo.create
      "findlib-loader"
      ~input:(module Input)
      (fun (db, name) -> Loader.lookup_and_load db name)
  ;;

  let find_root_package db name : (Dune_package.t, Unavailable_reason.t) result Memo.t =
    Memo.exec memo (db, name)
  ;;

  open Memo.O

  let find t name =
    Lib_name.package_name name
    |> find_root_package t
    >>| Result.bind ~f:(fun (p : Dune_package.t) ->
      match Lib_name.Map.find p.entries name with
      | Some x -> Ok x
      | None -> Error Unavailable_reason.Not_found)
  ;;

  let load_all_packages (t : DB.t) =
    Loader.root_packages t
    >>| Package.Name.Set.to_list
    >>= Memo.parallel_map ~f:(fun name ->
      let+ pkg = find_root_package t name in
      name, pkg)
  ;;

  let all_packages t =
    load_all_packages t
    >>| List.fold_left ~init:[] ~f:(fun acc (_, x) ->
      match x with
      | Ok (p : Dune_package.t) ->
        Lib_name.Map.fold p.entries ~init:acc ~f:(fun x acc -> x :: acc)
      | Error _ -> acc)
    >>| List.sort ~compare:(fun a b ->
      Lib_name.compare (Dune_package.Entry.name a) (Dune_package.Entry.name b))
  ;;

  let all_broken_packages t =
    load_all_packages t
    >>| List.fold_left ~init:[] ~f:(fun acc (name, x) ->
      match x with
      | Ok _ | Error Unavailable_reason.Not_found -> acc
      | Error (Invalid_dune_package exn) -> (name, exn) :: acc)
    >>| List.sort ~compare:(fun (a, _) (b, _) -> Package.Name.compare a b)
  ;;
end

module For_tests = struct
  let create ~paths ~lib_config = DB.create ~paths ~lib_config
end

type t = DB.t

let create_with_paths ~paths =
  Per_context.create_by_name ~name:"findlib" (fun context ->
    Memo.lazy_ (fun () ->
      let* context = Context.DB.get context in
      let* lib_config =
        let+ ocaml = Context.ocaml context in
        ocaml.lib_config
      in
      DB.create ~paths ~lib_config)
    |> Memo.Lazy.force)
  |> Staged.unstage
;;

let create context_name =
  let* context = Context.DB.get context_name in
  let* paths = Context.findlib_paths context in
  create_with_paths context_name ~paths
;;

include Public
