open Import
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

type t =
  { stdlib_dir : Path.t
  ; paths : Path.t list
  ; builtins : Meta.Simplified.t Package.Name.Map.t
  ; ext_lib : Filename.Extension.t
  }

let equal t { stdlib_dir; paths; builtins; ext_lib } =
  Path.equal t.stdlib_dir stdlib_dir
  && List.equal Path.equal t.paths paths
  && Package.Name.Map.equal ~equal:Meta.Simplified.equal t.builtins builtins
  && String.equal t.ext_lib ext_lib
;;

let hash { stdlib_dir; paths; builtins; ext_lib } =
  Poly.hash
    ( Path.hash stdlib_dir
    , List.hash Path.hash paths
    , Package.Name.Map.to_list builtins
      |> List.hash (fun (k, v) ->
        Tuple.T2.hash Package.Name.hash Meta.Simplified.hash (k, v))
    , String.hash ext_lib )
;;

let findlib_predicates_set_by_dune = Ps.of_list [ P.ppx_driver; P.mt; P.mt_posix ]

module Make_loader
    (Monad : sig
       type 'a t

       val return : 'a -> 'a t

       module List : sig
         val for_all : 'a list -> f:('a -> bool t) -> bool t
         val exists : 'a list -> f:('a -> bool t) -> bool t
         val fold_left : 'a list -> f:('acc -> 'a -> 'acc t) -> init:'acc -> 'acc t
         val find_map : 'a list -> f:('a -> 'b option t) -> 'b option t
       end

       module O : sig
         val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
         val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
         val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
         val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
       end
     end)
    (Fs : sig
       val dir_contents
         :  Path.t
         -> (Filename.t list, Unix_error.Detailed.t) result Monad.t

       val file_exists : Path.t -> bool Monad.t
       val dir_exists : Path.t -> bool Monad.t
       val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a Monad.t
     end) : sig
  (* Search for a <package>/{META,dune-package} file in the findlib search path *)
  val lookup_and_load
    :  t
    -> Package.Name.t
    -> (Dune_package.t, Unavailable_reason.t) result Monad.t
end = struct
  open Monad.O

  let mangled_module_re =
    lazy
      (let open Re in
       [ rep any; str "__"; rep any ] |> seq |> compile)
  ;;

  let to_dune_library (t : Findlib.Package.t) ~ext_lib =
    let loc = Loc.in_file t.meta_file in
    let add_loc x = loc, x in
    let dot_dune_file =
      Path.relative t.dir (sprintf "%s.dune" (Lib_name.to_string t.name))
    in
    let* dot_dune_exists = Fs.file_exists dot_dune_file in
    if dot_dune_exists
    then
      User_warning.emit
        ~loc:(Loc.in_file dot_dune_file)
        [ Pp.text
            ".dune files are ignored since 2.0. Reinstall the library with dune >= 2.0 \
             to get rid of this warning and enable support for the subsystem this \
             library provides."
        ];
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
    let+ (info : Path.t Lib_info.t) =
      let kind = Findlib.Package.kind t in
      let sub_systems = Sub_system_name.Map.empty in
      let synopsis = Findlib.Package.description t in
      let status =
        match Lib_name.analyze t.name with
        | Private (_, _) -> Lib_info.Status.Installed_private
        | Public (_, _) -> Lib_info.Status.Installed
      in
      let src_dir = Obj_dir.dir obj_dir in
      let version = Findlib.Package.version t in
      let dune_version = None in
      let virtual_deps = [] in
      let implements = None in
      let orig_src_dir = None in
      let main_module_name : Lib_info.Main_module_name.t = This None in
      let enabled = Lib_info.Enabled_status.Normal in
      let requires =
        Findlib.Package.requires t
        |> List.map ~f:(fun name -> Lib_dep.direct (add_loc name))
      in
      let ppx_runtime_deps = List.map ~f:add_loc (Findlib.Package.ppx_runtime_deps t) in
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
      let melange_runtime_deps = Lib_info.File_deps.External [] in
      let preprocess = Preprocess.Per_module.no_preprocessing () in
      let virtual_ = None in
      let default_implementation = None in
      let wrapped = None in
      let+ dir_contents = Fs.dir_contents t.dir in
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
           | _ :: _ as modules -> Ok (List.map ~f:Module_name.of_string modules)
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
                    if (* We add this hack to skip manually mangled
                          libraries *)
                       Re.execp (Lazy.force mangled_module_re) fname
                    then Ok None
                    else (
                      match
                        let name = Filename.chop_extension fname in
                        Module_name.of_string_user_error (Loc.in_dir src_dir, name)
                      with
                      | Ok s -> Ok (Some s)
                      | Error e -> Error e))))
      in
      let modules = Lib_info.Source.External None in
      Lib_info.create
        ~loc
        ~path_kind:External
        ~name:t.name
        ~kind
        ~status
        ~src_dir
        ~orig_src_dir
        ~obj_dir
        ~version
        ~synopsis
        ~main_module_name
        ~sub_systems
        ~requires
        ~foreign_objects
        ~public_headers
        ~plugins
        ~archives
        ~ppx_runtime_deps
        ~foreign_archives
        ~native_archives:(Files native_archives)
        ~foreign_dll_files:[]
        ~jsoo_runtime
        ~preprocess
        ~enabled
        ~virtual_deps
        ~dune_version
        ~virtual_
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
    in
    Dune_package.Lib.of_findlib info
  ;;

  module Exists = Findlib.Package.Exists (Monad) (Fs)

  (* Parse all the packages defined in a META file *)
  let dune_package_of_meta (db : t) ~dir ~meta_file ~(meta : Meta.Simplified.t) =
    let rec loop ~dir ~full_name (meta : Meta.Simplified.t) acc =
      let vars = Vars.of_meta_rules meta.vars in
      let pkg_dir = Vars.get vars "directory" Ps.empty in
      let dir =
        match pkg_dir with
        | None | Some "" -> dir
        | Some pkg_dir ->
          if pkg_dir.[0] = '+' || pkg_dir.[0] = '^'
          then Path.relative db.stdlib_dir (String.drop pkg_dir 1)
          else if Filename.is_relative pkg_dir
          then Path.relative dir pkg_dir
          else Path.of_filename_relative_to_initial_cwd pkg_dir
      in
      let pkg : Findlib.Package.t =
        { Findlib.Package.meta_file; name = full_name; dir; vars }
      in
      let* lib = to_dune_library pkg ~ext_lib:db.ext_lib in
      let* (entry : Dune_package.Entry.t) =
        let+ exists =
          Exists.exists
            pkg
            ~is_builtin:
              (Package.Name.Map.mem db.builtins (Lib_name.package_name pkg.name))
        in
        if exists then Dune_package.Entry.Library lib else Hidden_library lib
      in
      let acc = Lib_name.Map.add_exn acc (Dune_package.Entry.name entry) entry in
      Monad.List.fold_left meta.subs ~init:acc ~f:(fun acc (meta : Meta.Simplified.t) ->
        let full_name =
          match meta.name with
          | None -> full_name
          | Some name -> Lib_name.nest full_name name
        in
        loop ~dir ~full_name meta acc)
    in
    let name = Option.value_exn meta.name in
    let+ entries =
      loop ~dir ~full_name:(Option.value_exn meta.name) meta Lib_name.Map.empty
    in
    { Dune_package.name = Lib_name.package_name name
    ; version =
        (let open Option.O in
         let* e = Lib_name.Map.find entries name in
         Dune_package.Entry.version e)
    ; entries
    ; dir
    ; sections = Section.Map.empty
    ; sites = Site.Map.empty
    ; files = []
    }
  ;;

  let load_meta name file =
    Fs.file_exists file
    >>= function
    | false -> Monad.return None
    | true -> Fs.with_lexbuf_from_file file ~f:(Meta.of_lex ~name) >>| Option.some
  ;;

  let load_builtin db meta =
    dune_package_of_meta
      db
      ~dir:db.stdlib_dir
      ~meta_file:(Path.of_string "<internal>")
      ~meta
  ;;

  module Load_dune_package = Dune_package.Or_meta.Load (Monad) (Fs)

  let lookup db name dir : (Dune_package.t, Unavailable_reason.t) result option Monad.t =
    let load_meta ~dir meta_file =
      load_meta (Some name) meta_file
      >>= function
      | None -> Monad.return None
      | Some meta -> dune_package_of_meta db ~dir ~meta_file ~meta >>| Option.some
    in
    (* XXX DUNE4 why do we allow [META.foo] override [dune-package] file? *)
    Path.relative dir (Findlib.Package.meta_fn ^ "." ^ Package.Name.to_string name)
    |> load_meta ~dir
    >>= function
    | Some pkg -> Monad.return (Some (Ok pkg))
    | None ->
      let dir = Path.relative dir (Package.Name.to_string name) in
      Fs.dir_exists dir
      >>= (function
      | false -> Monad.return None
      | true ->
        (let dune = Path.relative dir Dune_package.fn in
         Fs.file_exists dune
         >>= function
         | true -> Load_dune_package.load dune
         | false -> Monad.return (Ok Dune_package.Or_meta.Use_meta))
        >>= (function
        | Error e ->
          Monad.return (Some (Error (Unavailable_reason.Invalid_dune_package e)))
        | Ok (Dune_package.Or_meta.Dune_package p) -> Monad.return (Some (Ok p))
        | Ok Use_meta ->
          Path.relative dir Findlib.Package.meta_fn
          |> load_meta ~dir
          >>| Option.map ~f:(fun pkg -> Ok pkg)))
  ;;

  let lookup_and_load (db : t) name =
    match Package.Name.Map.find db.builtins name with
    | Some meta ->
      (* XXX DUNE4 weird to favor these hardcoded packages over user possibly
         user defined libraries *)
      load_builtin db meta >>| Result.ok
    | None ->
      Monad.List.find_map db.paths ~f:(lookup db name)
      >>| (function
      | Some m -> m
      | None ->
        (match Package.Name.to_string name with
         | "dune" -> Ok builtin_for_dune
         | _ -> Error Unavailable_reason.Not_found))
  ;;
end

module Loader =
  Make_loader
    (Memo)
    (struct
      open Memo.O

      let dir_contents f =
        Path.as_outside_build_dir_exn f
        |> Fs_memo.dir_contents
        >>| Result.map ~f:(fun contents ->
          Fs_cache.Dir_contents.to_list contents |> List.map ~f:fst)
      ;;

      let file_exists f = Fs_memo.file_exists (Path.as_outside_build_dir_exn f)
      let dir_exists f = Fs_memo.dir_exists (Path.as_outside_build_dir_exn f)

      let with_lexbuf_from_file file ~f =
        Fs_memo.with_lexbuf_from_file (Path.as_outside_build_dir_exn file) ~f
      ;;
    end)

let memo =
  let module Input = struct
    type nonrec t = t * Package.Name.t

    let to_dyn = Dyn.opaque
    let hash = Tuple.T2.hash hash Package.Name.hash
    let equal = Tuple.T2.equal equal Package.Name.equal
  end
  in
  Memo.create
    "findlib"
    ~input:(module Input)
    (fun (db, name) -> Loader.lookup_and_load db name)
;;

let find_root_package db name : (Dune_package.t, Unavailable_reason.t) result Memo.t =
  Memo.exec memo (db, name)
;;

open Memo.O

let find t name =
  let+ p = find_root_package t (Lib_name.package_name name) in
  let open Result.O in
  let* p = p in
  match Lib_name.Map.find p.entries name with
  | Some x -> Ok x
  | None -> Error Unavailable_reason.Not_found
;;

let root_packages (db : t) =
  let+ pkgs =
    Memo.List.concat_map db.paths ~f:(fun dir ->
      Fs_memo.dir_contents (Path.as_outside_build_dir_exn dir)
      >>= function
      | Error (ENOENT, _, _) -> Memo.return []
      | Error (unix_error, _, _) ->
        User_error.raise
          [ Pp.textf
              "Unable to read directory %s for findlib package"
              (Path.to_string_maybe_quoted dir)
          ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
          ]
      | Ok dir_contents ->
        let dir_contents = Fs_cache.Dir_contents.to_list dir_contents in
        Memo.List.filter_map dir_contents ~f:(fun (name, _) ->
          let+ exists =
            Fs_memo.file_exists
              (Path.as_outside_build_dir_exn
                 (Path.L.relative dir [ name; Findlib.Package.meta_fn ]))
          in
          if exists then Some (Package.Name.of_string name) else None))
    >>| Package.Name.Set.of_list
  in
  let builtins = Package.Name.Set.of_list (Package.Name.Map.keys db.builtins) in
  Package.Name.Set.union pkgs builtins
;;

let load_all_packages (t : t) =
  root_packages t
  >>| Package.Name.Set.to_list
  >>= Memo.parallel_map ~f:(fun name ->
    let+ pkg = find_root_package t name in
    name, pkg)
;;

let all_packages t =
  let+ root_packages = load_all_packages t in
  List.fold_left root_packages ~init:[] ~f:(fun acc (_, x) ->
    match x with
    | Ok (p : Dune_package.t) ->
      Lib_name.Map.fold p.entries ~init:acc ~f:(fun x acc -> x :: acc)
    | Error _ -> acc)
  |> List.sort ~compare:(fun a b ->
    Lib_name.compare (Dune_package.Entry.name a) (Dune_package.Entry.name b))
;;

let all_broken_packages t =
  let+ packages = load_all_packages t in
  List.fold_left packages ~init:[] ~f:(fun acc (name, x) ->
    match x with
    | Ok _ | Error Unavailable_reason.Not_found -> acc
    | Error (Invalid_dune_package exn) -> (name, exn) :: acc)
  |> List.sort ~compare:(fun (a, _) (b, _) -> Package.Name.compare a b)
;;

let create ~paths ~(lib_config : Lib_config.t) =
  let stdlib_dir = lib_config.stdlib_dir in
  let version = lib_config.ocaml_version in
  let ext_lib = lib_config.ext_lib in
  let+ builtins = Meta.builtins ~stdlib_dir ~version in
  { stdlib_dir; paths; builtins; ext_lib }
;;

module For_tests = struct
  let create = create
end

let create =
  Context.DB.create_db ~name:"findlib" (fun context ->
    create ~paths:context.findlib_paths ~lib_config:context.ocaml.lib_config)
  |> Staged.unstage
;;
