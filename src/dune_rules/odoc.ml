open Import
open Memo.O

let ( ++ ) = Path.Build.relative

let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")

let lib_equal l1 l2 = Lib.compare l1 l2 |> Ordering.is_eq

let find_project_by_key =
  let memo =
    let make_map projects =
      Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
          (Dune_project.file_key project, project))
      |> Memo.return
    in
    let module Input = struct
      type t = Dune_project.t list

      let equal = List.equal Dune_project.equal

      let hash = List.hash Dune_project.hash

      let to_dyn = Dyn.list Dune_project.to_dyn
    end in
    Memo.create "project-by-keys" ~input:(module Input) make_map
  in
  fun key ->
    let* { projects; _ } = Dune_load.load () in
    let+ map = Memo.exec memo projects in
    Dune_project.File_key.Map.find_exn map key

module Scope_key : sig
  val of_string : Context.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t

  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      (Lib_name.parse_string_exn (Loc.none, s), public_libs)
    | Some (lib, key) ->
      let+ scope =
        let key = Dune_project.File_key.of_string key in
        find_project_by_key key >>= Scope.DB.find_by_project context
      in
      (Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope)

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib)
      (Dune_project.File_key.to_string key)
end

let is_public lib =
  let lib = Lib.Local.to_lib lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private -> false
  | Installed -> true
  | Public _ -> true
  | Private (_project, _) -> false

let lib_unique_name lib =
  let lib = Lib.Local.to_lib lib in
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project

module Paths = struct
  let odoc_support_dirname = "docs/odoc.support"

  let root (context : Context.t) all =
    let sub = if all then "_doc/all" else "_doc" in
    Path.Build.relative context.Context.build_dir sub

  let local_path_of_findlib_path ctx obj_dir =
    List.find_map ctx.Context.findlib_paths ~f:(fun p ->
        if Path.is_descendant ~of_:p obj_dir then
          Some (Path.reach obj_dir ~from:p)
        else None)
    |> Option.value_exn

  let html_root ctx all = root ctx all ++ "html"

  let odoc_support ctx all = html_root ctx all ++ odoc_support_dirname
end

module Index = struct
  type external_fallback = EF of string

  type t =
    | Toplevel
    | LocalPackage of Package.Name.t
    | PrivateLib of string
    | ExternalDunePackage of Package.Name.t
    | ExternalFallback of external_fallback

  let to_dyn x =
    let open Dyn in
    match x with
    | Toplevel -> variant "Toplevel" []
    | LocalPackage pkg -> variant "LocalPackage" [ Package.Name.to_dyn pkg ]
    | PrivateLib lnu -> variant "PrivateLib" [ String lnu ]
    | ExternalDunePackage pkg ->
      variant "ExternalDunePackage" [ Package.Name.to_dyn pkg ]
    | ExternalFallback (EF l) -> variant "ExternalFallback" [ String l ]

  let compare x y = Dyn.compare (to_dyn x) (to_dyn y)

  let top_dir_of_external_fallback = function
    | EF s -> String.split ~on:'/' s |> List.hd

  let obj_dir ctx all : t -> Path.Build.t =
    let root = Paths.root ctx all in
    function
    | Toplevel -> root ++ "index"
    | LocalPackage p -> root ++ "index" ++ "local" ++ Package.Name.to_string p
    | ExternalDunePackage p ->
      root ++ "index" ++ "external" ++ Package.Name.to_string p
    | ExternalFallback d ->
      root ++ "index" ++ "external" ++ top_dir_of_external_fallback d
    | PrivateLib lnu -> root ++ "index" ++ "private" ++ lnu

  let html_dir ctx all (m : t) =
    let root = Paths.html_root ctx all ++ "docs" in
    match m with
    | Toplevel -> root
    | ExternalDunePackage pkg | LocalPackage pkg ->
      root ++ Package.Name.to_string pkg
    | PrivateLib lnu -> root ++ lnu
    | ExternalFallback p -> root ++ top_dir_of_external_fallback p

  let mld_name : t -> string = function
    | Toplevel -> "docs"
    | ExternalDunePackage pkg | LocalPackage pkg -> Package.Name.to_string pkg
    | PrivateLib s -> s
    | ExternalFallback ef -> top_dir_of_external_fallback ef

  let mld_filename index = mld_name index ^ ".mld"

  let mld_path ctx all index = obj_dir ctx all index ++ mld_filename index
end

module IndexSet = Set.Make (Index) (Map.Make (Index))

module Target = struct
  type module_source = Path.Build.t * bool

  type mld_source = Path.Build.t

  type source =
    | Module : (Path.t * bool) -> source
    | Mld : Path.t -> source

  (** The target tells us the locations of the artefacts - input cmtis/mlds,
      odoc files, odocl files and html file. It is parameterised over a type
      related to where the input files will be found. *)
  type _ t =
    | Lib : Lib.Local.t -> module_source t
        (** [Lib lib] represents a library in the source tree. The only inputs
            associated with libraries are modules as mld files are always
            associated with the whole package *)
    | Pkg : Package.Name.t -> mld_source t
        (** [Pkg pkg_name] represents a package defined in the source tree.
            There are no modules, only mld files associated with the package. *)
    | ExtLib : string -> source t
        (** An external library. We don't distinguish between libraries and
            packages for external libs as all the files (ie, odoc files from
            both modules and mlds) end up in the same directory. *)
    | Index : Index.t -> unit t
        (** The indices for each type of tree go into separate dirs. *)

  type dirs =
    { odocs : Path.Build.t
    ; html : Path.Build.t
    }

  let odocs_dir : type a. Context.t -> bool -> a t -> Path.Build.t =
   fun ctx all a ->
    match a with
    | Lib lib ->
      if all then
        Paths.root ctx all ++ sprintf "odoc/internal/%s" (lib_unique_name lib)
      else
        let obj_dir = Lib.Local.obj_dir lib in
        Obj_dir.odoc_dir obj_dir
    | Pkg pkg ->
      Paths.root ctx all ++ sprintf "odoc/pkg/%s" (Package.Name.to_string pkg)
    | ExtLib p -> Paths.root ctx all ++ sprintf "odoc/external/%s" p
    | Index i -> Index.obj_dir ctx all i

  let html_target_dir : type a. Context.t -> bool -> a t -> Path.Build.t =
   fun ctx all t ->
    let root = Paths.html_root ctx all ++ "docs" in
    match t with
    | Pkg pkg -> root ++ Package.Name.to_string pkg
    | Lib lib -> (
      match Lib_info.package (Lib.Local.info lib) with
      | Some pkg -> root ++ Package.Name.to_string pkg
      | None -> root ++ lib_unique_name lib)
    | ExtLib dir -> root ++ (String.split_on_char ~sep:'/' dir |> List.hd)
    | Index i -> Index.html_dir ctx all i

  let dirs ctx all t =
    { odocs = odocs_dir ctx all t; html = html_target_dir ctx all t }
end

let add_rule sctx =
  let dir = (Super_context.context sctx).build_dir in
  Super_context.add_rule sctx ~dir

(* Returns a map from a 'local dir' - defined as a directory in
   your switch's lib dir, e.g. 'ocaml' or 'dune' - to a map from
   subdir to a map from library name to Dune_package.Lib.t. For
   example, on 4.14.1, looking up "ocaml" in the map gives a map
   containing the following:

   [ ocaml ->
     [ bigarray -> <lib>, bytes -> <lib>, dynlink -> <lib> ... ]
   [ ocaml/compiler-libs ->
     [ compiler-libs -> <lib>, compiler-libs.bytecomp -> <lib> ... ]
*)
let libs_of_local_dir_def =
  let f (ctx : Context.t) =
    let* findlib =
      Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
    in
    let* all_packages = Findlib.all_packages findlib in
    let* db = Scope.DB.public_libs ctx in
    let map =
      List.fold_left all_packages ~init:(Memo.return String.Map.empty)
        ~f:(fun map entry ->
          let* map = map in
          match entry with
          | Dune_package.Entry.Library l -> (
            let obj_dir =
              Dune_package.Lib.info l |> Lib_info.obj_dir |> Obj_dir.dir
            in
            let local = Paths.local_path_of_findlib_path ctx obj_dir in
            let toplocal = String.split local ~on:'/' |> List.hd in
            let name = Dune_package.Lib.info l |> Lib_info.name in
            let+ lib_opt = Lib.DB.find db name in
            match lib_opt with
            | None -> map
            | Some lib ->
              let update_fn = function
                | Some libs ->
                  Some
                    (String.Map.update libs local ~f:(function
                      | Some libs ->
                        Some (Lib_name.Map.add_exn libs name (l, lib))
                      | None -> Some (Lib_name.Map.singleton name (l, lib))))
                | None ->
                  Some
                    (String.Map.singleton local
                       (Lib_name.Map.singleton name (l, lib)))
              in
              String.Map.update map toplocal ~f:update_fn)
          | _ -> Memo.return map)
    in
    map
  in
  let module Input = Context in
  Memo.create "libs_of_local_dir" ~input:(module Input) f

let libs_of_local_dir ctx = Memo.exec libs_of_local_dir_def ctx

(* The following function gives information about how we can document a
   particular external directory. Call with the a top-level path, e.g. "base".
   If 'base' is a dune package, where we know all the info (e.g. a wrapped
   library), and in addition all other libraries under the [base] path are the
   same, this will return some extracted info from all of the libraries in the
   package. If not, this will return a fallback with everything found under
   the [base] path.

   Note that we can't just call [Findlib.find_root_package] as that won't
   work for paths like [ocaml] *)

exception Fallback

type dune_with_modules =
  { local_dir : string
  ; lib : Lib.t
  ; modules : Modules.t
  ; entry_modules : Module_name.t list
  }

type fallback =
  { libs : (Dune_package.Lib.t * Lib.t) Lib_name.Map.t Import.String.Map.t }

type local_dir_type =
  | Nothing
  | Dune_with_modules of (Package.Name.t * dune_with_modules list)
  | Fallback of fallback

let classify_local_dir_memo =
  let run (ctx, local_dir) =
    (match String.index_opt local_dir '/' with
    | Some _ -> assert false
    | None -> ());
    let pkg = local_dir in
    let* map = libs_of_local_dir ctx in
    match String.Map.find map pkg with
    | None ->
      Log.info
        [ Pp.textf "classify_local_dir: No lib at this path: %s" local_dir ];
      Memo.return Nothing
    | Some libs -> (
      let* public_libs = Lib.DB.installed ctx ~host:None in
      try
        let f local_dir libs acc =
          match Lib_name.Map.values libs with
          | [ (lib, _) ] -> (
            let info = Dune_package.Lib.info lib in
            let mods_opt = Lib_info.modules (Dune_package.Lib.info lib) in
            match (mods_opt, Lib_info.entry_modules info) with
            | External (Some modules), External (Ok entry_modules) ->
              (local_dir, lib, modules, entry_modules) :: acc
            | _ -> raise Fallback)
          | _ -> raise Fallback
        in
        let ms = String.Map.foldi libs ~f ~init:[] in
        let* ms =
          Memo.List.map ms
            ~f:(fun (local_dir, dune_package_lib, modules, entry_modules) ->
              let info = Dune_package.Lib.info dune_package_lib in
              (* let requires = Resolve.return requires in *)
              let* resolved_lib =
                Lib.DB.resolve public_libs (Loc.none, Lib_info.name info)
              in
              let+ lib = Resolve.read_memo resolved_lib in
              let package = Lib_info.package info |> Option.value_exn in
              (package, { local_dir; lib; modules; entry_modules }))
        in
        let pkg =
          List.fold_left ~init:None ms ~f:(fun acc m ->
              match acc with
              | None -> Some (fst m)
              | Some p -> if p <> fst m then raise Fallback else acc)
          |> Option.value_exn
        in
        Memo.return (Dune_with_modules (pkg, List.map ~f:snd ms))
      with Fallback -> Memo.return (Fallback { libs }))
  in
  let module Input = struct
    type t = Context.t * string

    let equal (c1, s1) (c2, s2) = Context.equal c1 c2 && String.equal s1 s2

    let hash (c, s) = Poly.hash (Context.hash c, String.hash s)

    let to_dyn = Dyn.pair Context.to_dyn Dyn.string
  end in
  Memo.create "libs_and_packages" ~input:(module Input) run

let classify_local_dir ctx dir = Memo.exec classify_local_dir_memo (ctx, dir)

module Valid = struct
  (* These functions return a whitelist of libraries and packages that
      should be documented. There is one single function that performs this
      task because there needs to be an exact correspondance at various points
     in the process - e.g. the indexes need to know exactly which libraries will
     be documented and where. *)
  let valid_libs_and_packages =
    let run (ctx, all, projects) =
      let* mask = Only_packages.get_mask () in
      let mask = Option.map ~f:Package.Name.Map.keys mask in

      let* libs_and_pkgs =
        Scope.DB.with_all ctx ~f:(fun find ->
            Memo.List.fold_left ~init:([], [])
              ~f:(fun (libs_acc, pkg_acc) proj ->
                let* vendored =
                  Source_tree.is_vendored (Dune_project.root proj)
                in
                if vendored then Memo.return (libs_acc, pkg_acc)
                else
                  let scope = find proj in
                  let lib_db = Scope.libs scope in
                  let+ libs = Lib.DB.all lib_db in
                  let libs =
                    match mask with
                    | None -> libs
                    | Some mask ->
                      Lib.Set.filter
                        ~f:(fun lib ->
                          let info = Lib.info lib in
                          match Lib_info.package info with
                          | Some p -> List.mem ~equal:Package.Name.equal mask p
                          | None -> false)
                        libs
                  in
                  let libs_acc = (proj, lib_db, libs) :: libs_acc in
                  let pkgs =
                    let proj_pkgs =
                      Dune_project.packages proj |> Package.Name.Map.keys
                    in
                    match mask with
                    | Some m ->
                      List.filter
                        ~f:(List.mem ~equal:Package.Name.equal m)
                        proj_pkgs
                    | None -> proj_pkgs
                  in
                  let pkg_acc = pkgs @ pkg_acc in
                  (libs_acc, pkg_acc))
              projects)
      in
      let* libs, packages = libs_and_pkgs in

      let* stdlib = stdlib_lib ctx in

      let+ libs_list =
        Memo.all
          (List.map libs ~f:(fun (_, _lib_db, libs) ->
               Lib.Set.fold ~init:(Memo.return []) libs ~f:(fun lib acc ->
                   let* acc = acc in
                   let* libs =
                     Lib.closure (lib :: Option.to_list stdlib) ~linking:false
                   in
                   let+ libs = Resolve.read_memo libs in
                   libs :: acc)))
      in

      let libs_list =
        List.concat (List.concat libs_list)
        |> Lib.Set.of_list |> Lib.Set.to_list
      in

      let libs_list =
        List.filter libs_list ~f:(fun lib ->
            let is_impl =
              Lib.info lib |> Lib_info.implements |> Option.is_some
            in
            not is_impl)
      in

      let libs_list =
        if all then libs_list
        else
          List.filter libs_list ~f:(fun lib ->
              match Lib.Local.of_lib lib with
              | None -> false
              | Some l -> is_public l)
      in

      (libs_list, packages)
    in
    let module Input = struct
      type t = Context.t * bool * Dune_project.t list

      let equal (c1, b1, ps1) (c2, b2, ps2) =
        Context.equal c1 c2 && List.equal Dune_project.equal ps1 ps2 && b1 = b2

      let hash (c, b, ps) =
        Poly.hash (Context.hash c, b, List.hash Dune_project.hash ps)

      let to_dyn _ = Dyn.Opaque
    end in
    Memo.create "libs_and_packages" ~input:(module Input) run

  let get ctx all =
    let* { projects; _ } = Dune_load.load () in
    Memo.exec valid_libs_and_packages (ctx, all, projects)

  let filter_libs ctx all libs =
    let+ valid_libs, _ = get ctx all in
    List.filter libs ~f:(fun l -> List.mem valid_libs l ~equal:lib_equal)

  let is_valid ctx all lib =
    let+ valid_libs, _ = get ctx all in
    List.mem valid_libs lib ~equal:lib_equal

  let filter_fallback_libs ctx all libs =
    let+ valid_libs, _ = get ctx all in
    Import.String.Map.filter_map libs ~f:(fun libs ->
        let filtered =
          Lib_name.Map.filter libs ~f:(fun (_, l) ->
              List.mem valid_libs l ~equal:lib_equal)
        in
        if Lib_name.Map.cardinal filtered > 0 then Some filtered else None)
end

module Dep : sig
  (** [html_alias ctx target] returns the alias that depends on all html targets
      produced by odoc for [target] *)
  val html_alias : Context.t -> bool -> _ Target.t -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps :
       Context.t
    -> bool
    -> Lib.t list
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> bool -> 'a Target.t -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias ctx all t = Alias.doc ~dir:(Target.html_target_dir ctx all t)

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx all valid_libs pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p ->
           Dep.Set.singleton
             (Dep.alias (alias ~dir:(Target.odocs_dir ctx all (Pkg p))))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
           if not (List.mem ~equal:lib_equal valid_libs lib) then acc
           else
             match Lib.Local.of_lib lib with
             | None ->
               let obj_dir =
                 Lib.info lib |> Lib_info.obj_dir |> Obj_dir.obj_dir
               in
               let local_path = Paths.local_path_of_findlib_path ctx obj_dir in
               let dir = Target.odocs_dir ctx all (ExtLib local_path) in
               let alias = alias ~dir in
               Dep.Set.add acc (Dep.alias alias)
             | Some lib ->
               let dir = Target.odocs_dir ctx all (Lib lib) in
               let alias = alias ~dir in
               Dep.Set.add acc (Dep.alias alias)))

  let alias ctx all m = alias ~dir:(Target.odocs_dir ctx all m)

  let setup_deps ctx all m files =
    Rules.Produce.Alias.add_deps (alias ctx all m)
      (Action_builder.path_set files)
end

module Artefact : sig
  type artefact_ty =
    | Module of bool
    | Mld

  type t

  val odoc_file : t -> Path.Build.t

  val odocl_file : t -> Path.Build.t

  val html_dir : t -> Path.Build.t

  val html_file : t -> Path.Build.t

  val artefact_ty : t -> artefact_ty

  val source_file : t -> Path.t

  val odocs_dir : t -> Path.Build.t

  val is_visible : t -> bool

  val module_name : t -> Module_name.t option

  val reference : t -> string

  val make : Context.t -> bool -> 'a Target.t -> 'a -> t
end = struct
  type artefact_ty =
    | Module of bool
    | Mld

  type filenames =
    { source : Path.t
    ; odoc : string
    ; html : string (* nb, this might have a dir too, ie Foo/index.html *)
    }

  type t = Target.dirs * filenames * artefact_ty

  let odoc_file (ds, fs, _ty) = ds.Target.odocs ++ fs.odoc

  let odocl_file (ds, fs, _ty) = ds.Target.odocs ++ (fs.odoc ^ "l")

  let html_dir (ds, _fs, _ty) = ds.Target.html

  let html_file (ds, fs, _ty) = ds.Target.html ++ fs.html

  let odocs_dir (ds, _, _) = ds.Target.odocs

  let artefact_ty (_ds, _fs, ty) = ty

  let source_file (_ds, fs, _ty) = fs.source

  let is_visible (_ds, _fs, ty) =
    match ty with
    | Module x -> x
    | Mld -> true

  let module_name (_ds, fs, ty) =
    match ty with
    | Module _ ->
      let basename =
        Path.basename fs.source |> Filename.chop_extension
        |> Stdune.String.capitalize
      in
      Some (Module_name.of_string_allow_invalid (Loc.none, basename))
    | _ -> None

  let reference (_ds, fs, ty) =
    match ty with
    | Mld ->
      let basename = Path.basename fs.source |> Filename.chop_extension in
      sprintf "page-\"%s\"" basename
    | Module _ ->
      let basename =
        Path.basename fs.source |> Filename.chop_extension
        |> Stdune.String.capitalize
      in
      sprintf "module-%s" basename

  let make : type a. Context.t -> bool -> a Target.t -> a -> t =
   fun ctx all target source ->
    let dirs = Target.dirs ctx all target in
    let module_files source ty =
      let basename =
        Path.basename source |> Filename.chop_extension
        |> Stdune.String.uncapitalize
      in
      let odoc = basename ^ ".odoc" in
      let html = "index.html" in
      ( { dirs with html = dirs.html ++ Stdune.String.capitalize basename }
      , { odoc; html; source }
      , ty )
    in
    let mld_files source ty is_index =
      let basename = Path.basename source |> Filename.chop_extension in
      let odoc = "page-" ^ basename ^ ".odoc" in
      let html =
        if is_index then "index.html" else sprintf "%s.html" basename
      in
      (dirs, { odoc; html; source }, ty)
    in
    match target with
    | Lib _ ->
      let path, visibility = source in
      module_files (Path.build path) (Module visibility)
    | Pkg _ -> mld_files (Path.build source) Mld false
    | ExtLib _ -> (
      match (source : Target.source) with
      | Target.Module (path, visibility) ->
        module_files path (Module visibility)
      | Target.Mld path -> mld_files path Mld false)
    | Index index ->
      let filename = Index.mld_filename index in
      let dir = Index.obj_dir ctx all index in
      let source = Path.build (dir ++ filename) in
      mld_files source Mld true
end

let odoc_base_flags sctx build_dir =
  let open Memo.O in
  let+ conf = Super_context.odoc sctx ~dir:build_dir in
  match conf.Env_node.Odoc.warnings with
  | Fatal -> Command.Args.A "--warn-error"
  | Nonfatal -> S []

let odoc_program sctx dir =
  Super_context.resolve_program sctx ~dir "odoc" ~loc:None
    ~hint:"opam install odoc"

let run_odoc sctx ~dir command ~flags_for args =
  let build_dir = (Super_context.context sctx).build_dir in
  let open Memo.O in
  let* program = odoc_program sctx build_dir in
  let+ base_flags =
    match flags_for with
    | None -> Memo.return Command.Args.empty
    | Some path -> odoc_base_flags sctx path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run ~dir program [ A command; base_flags; S args ]

let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
    let+ deps =
      if Module.has m ~ml_kind:Intf then Dep_graph.deps_of dep_graphs.intf m
      else
        (* When a module has no .mli, use the dependencies for the .ml *)
        Dep_graph.deps_of dep_graphs.impl m
    in
    List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))

let parse_odoc_deps lines =
  let rec getdeps cur = function
    | x :: rest -> (
      match String.split ~on:' ' x with
      | [ m; hash ] -> getdeps ((Module_name.of_string m, hash) :: cur) rest
      | _ -> getdeps cur rest)
    | [] -> cur
  in
  getdeps [] lines

let parent_args parent_opt =
  match parent_opt with
  | None -> []
  | Some mld ->
    let dir = Artefact.odocs_dir mld in
    let reference = Artefact.reference mld in
    let odoc_file =
      Artefact.odoc_file mld |> Path.build |> Dune_engine.Dep.file
      |> Dune_engine.Dep.Set.singleton
    in
    Command.Args.
      [ A "-I"
      ; Path (Path.build dir)
      ; A "--parent"
      ; A reference
      ; Hidden_deps odoc_file
      ]

let odoc_include_flags ctx all pkg requires indices =
  Resolve.args
    (let open Resolve.O in
    let+ libs = requires in
    let paths =
      List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
          match Lib.Local.of_lib lib with
          | None ->
            let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.obj_dir in
            let local_path = Paths.local_path_of_findlib_path ctx obj_dir in
            Path.Set.add paths
              (Path.build (Target.odocs_dir ctx all (ExtLib local_path)))
          | Some lib ->
            Path.Set.add paths
              (Path.build (Target.odocs_dir ctx all (Lib lib))))
    in
    let paths =
      match pkg with
      | Some p ->
        Path.Set.add paths (Path.build (Target.odocs_dir ctx all (Pkg p)))
      | None -> paths
    in
    let paths =
      List.fold_left indices ~init:paths ~f:(fun p index ->
          let odoc_dir = Target.odocs_dir ctx all (Index index) in
          Path.Set.add p (Path.build odoc_dir))
    in
    Command.Args.S
      (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
           [ Command.Args.A "-I"; Path dir ])))

let create_index_odoc ctx all index = Artefact.make ctx all (Index index) ()

let index_dep ctx all index =
  let a = create_index_odoc ctx all index in
  Artefact.odoc_file a |> Path.build |> Dune_engine.Dep.file
  |> Dune_engine.Dep.Set.singleton

let compile_module sctx all ~artefact:a ~requires ~package ~module_deps
    ~parent_opt ~indices =
  let odoc_file = Artefact.odoc_file a in
  let open Memo.O in
  let cmti = Artefact.source_file a in
  let ctx = Super_context.context sctx in
  let iflags =
    Command.Args.memo (odoc_include_flags ctx all package requires indices)
  in
  let* valid_libs, _ = Valid.get ctx all in
  let file_deps = Dep.deps ctx all valid_libs package requires in
  let parent_args = parent_args parent_opt in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.parent_exn (Path.build (Artefact.odoc_file a)) in
      let+ run_odoc =
        run_odoc sctx ~dir:doc_dir "compile" ~flags_for:(Some odoc_file)
          ([ Command.Args.A "-I"
           ; Path doc_dir
           ; iflags
           ; A "-o"
           ; Target odoc_file
           ; Dep cmti
           ]
          @ parent_args)
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets module_deps
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  odoc_file

let compile_requires libs =
  let+ requires = Memo.List.map ~f:Lib.requires libs in
  let requires = Resolve.all requires |> Resolve.map ~f:List.flatten in
  Resolve.map requires
    ~f:(List.filter ~f:(fun l -> not (List.mem libs l ~equal:lib_equal)))

let link_requires libs = Lib.closure libs ~linking:false

let compile_mld sctx a ~doc_dir ~parent_opt ~is_index ~children =
  assert (Artefact.artefact_ty a = Artefact.Mld);
  let odoc_file = Artefact.odoc_file a in
  let odoc_input = Artefact.source_file a in
  let parent_args =
    match parent_opt with
    | None -> []
    | _ -> parent_args parent_opt
  in
  let child_args =
    List.fold_left children ~init:[] ~f:(fun args child ->
        match Artefact.artefact_ty child with
        | Module true | Mld -> "--child" :: Artefact.reference child :: args
        | Module false -> args)
  in
  let child_args =
    if is_index && List.is_empty child_args then [ "--child"; "dummy" ]
    else child_args
  in

  let* run_odoc =
    run_odoc sctx ~dir:(Path.build doc_dir) "compile"
      ~flags_for:(Some odoc_file)
      (A "-o" :: Target odoc_file :: Dep odoc_input :: As child_args
     :: parent_args)
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file

let link_odoc_rules sctx all (artefacts : Artefact.t list) ~package ~libs
    ~indices =
  let ctx = Super_context.context sctx in
  let* requires = link_requires libs in
  let* valid_libs, _ = Valid.get ctx all in
  let deps = Dep.deps ctx all valid_libs package requires in
  let index_deps =
    List.map
      ~f:(fun x -> Command.Args.Hidden_deps (index_dep ctx all x))
      indices
  in
  let open Memo.O in
  Memo.List.iter artefacts ~f:(fun a ->
      let* run_odoc =
        run_odoc sctx
          ~dir:(Path.parent_exn (Path.build (Artefact.odocl_file a)))
          "link"
          ~flags_for:(Some (Artefact.odoc_file a))
          (index_deps
          @ [ odoc_include_flags ctx all package requires indices
            ; A "-o"
            ; Target (Artefact.odocl_file a)
            ; Dep (Path.build (Artefact.odoc_file a))
            ])
      in
      add_rule sctx
        (let open Action_builder.With_targets.O in
        Action_builder.with_no_targets deps >>> run_odoc))

let html_generate sctx all (a : Artefact.t) =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let odoc_support_path = Paths.odoc_support ctx all in
  let html_output = Paths.html_root ctx all in
  let support_relative =
    Path.reach (Path.build odoc_support_path) ~from:(Path.build html_output)
  in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build html_output) "html-generate" ~flags_for:None
      [ A "-o"
      ; Path (Path.build html_output)
      ; A "--support-uri"
      ; A support_relative
      ; A "--theme-uri"
      ; A support_relative
      ; Dep (Path.build (Artefact.odocl_file a))
      ]
  in
  let rule, result =
    match Artefact.artefact_ty a with
    | Mld ->
      ( Action_builder.With_targets.add
          ~file_targets:[ Artefact.html_file a ]
          run_odoc
      , None )
    | Module _ ->
      let dir = Artefact.html_dir a in
      ( Action_builder.With_targets.add_directories ~directory_targets:[ dir ]
          run_odoc
      , Some dir )
  in
  let+ () = add_rule sctx rule in
  result

let setup_library_odoc_rules cctx (local_lib : Lib.Local.t) =
  let open Memo.O in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let* requires = Compilation_context.requires_compile cctx in
  let* stdlib = stdlib_lib ctx in

  let requires =
    match stdlib with
    | None -> requires
    | Some lib -> Resolve.map requires ~f:(fun r -> lib :: r)
  in
  let info = Lib.Local.info local_lib in
  let package = Lib_info.package info in
  let modes = Lib_info.modes info in
  let mode = Lib_mode.Map.Set.for_merlin modes in
  let target = Target.Lib local_lib in
  let parent =
    let idx : Index.t =
      match package with
      | Some package -> LocalPackage package
      | None -> PrivateLib (lib_unique_name local_lib)
    in
    create_index_odoc ctx false idx
  in
  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let modules_and_odoc_files =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        try
          let entry_modules = Modules.entry_modules modules in
          let visible =
            List.mem entry_modules m ~equal:(fun m1 m2 ->
                Module_name.equal (Module.name m1) (Module.name m2))
          in
          let module_deps =
            module_deps m ~obj_dir
              ~dep_graphs:(Compilation_context.dep_graphs cctx)
          in
          let cm_kind =
            let open Lib_mode in
            match mode with
            | Ocaml _ -> Cm_kind.Ocaml Cmi
            | Melange -> Cm_kind.Melange Cmi
          in
          let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind m in
          let artefact = Artefact.make ctx false target (cmti_file, visible) in
          let parent_opt = if visible then Some parent else None in
          let compiled =
            let* c =
              compile_module sctx false ~artefact ~requires ~package
                ~module_deps ~parent_opt ~indices:[]
            in
            let+ () =
              if visible then
                link_odoc_rules sctx false [ artefact ] ~package
                  ~libs:[ (local_lib :> Lib.t) ]
                  ~indices:[]
              else Memo.return ()
            in
            c
          in
          compiled :: acc
        with _ -> acc)
  in
  let* modules_and_odoc_files = Memo.all_concurrently modules_and_odoc_files in

  Dep.setup_deps ctx false (Lib local_lib)
    (Path.Set.of_list_map modules_and_odoc_files ~f:(fun p -> Path.build p))

let setup_css_rule sctx all =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx all in
  let* run_odoc =
    let+ cmd =
      run_odoc sctx ~dir:(Path.build ctx.build_dir) "support-files"
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    cmd
    |> Action_builder.With_targets.add_directories ~directory_targets:[ dir ]
  in
  add_rule sctx run_odoc

let libs_of_pkg ctx ~pkg =
  let+ entries = Scope.DB.lib_entries_of_package ctx pkg in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map entries ~f:(fun (entry : Scope.DB.Lib_entry.t) ->
      match entry with
      | Library lib ->
        let is_impl =
          Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements
          |> Option.is_some
        in
        Option.some_if (not is_impl) lib
      | Deprecated_library_name _ -> None)

let modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let name = Lib.name (Lib.Local.to_lib lib) in
  Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
  >>| Ml_sources.modules ~for_:(Library name)

let entry_modules_by_lib sctx lib =
  modules_by_lib sctx lib >>| Modules.entry_modules

let entry_modules sctx ~pkg =
  let* l =
    libs_of_pkg (Super_context.context sctx) ~pkg
    >>| List.filter ~f:(fun lib ->
            Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private
            |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
        let+ m = entry_modules_by_lib sctx l in
        (l, m))
  in
  Lib.Local.Map.of_list_exn l

let static_html ctx all =
  let open Paths in
  [ odoc_support ctx all ]

let modules_of_dir d :
    (Module_name.t * (string * Path.t * [ `Cmti | `Cmt | `Cmi ])) list Memo.t =
  let extensions = [ (".cmti", `Cmti); (".cmt", `Cmt); (".cmi", `Cmi) ] in

  let* dir_res = Fs_memo.dir_contents (Path.as_outside_build_dir_exn d) in
  match dir_res with
  | Error _ -> Memo.return []
  | Ok dc ->
    let list = Fs_cache.Dir_contents.to_list dc in
    let modules =
      List.filter_map
        ~f:(fun (x, ty) ->
          match (ty, List.assoc extensions (Filename.extension x)) with
          | Unix.S_REG, Some _ -> Some (Filename.chop_extension x)
          | _, _ -> None)
        list
      |> List.sort_uniq ~compare:String.compare
    in
    let ms =
      List.map
        ~f:(fun m ->
          let ext, ty =
            List.find_exn
              ~f:(fun (ext, _ty) ->
                List.exists list ~f:(fun (n, _) -> n = m ^ ext))
              extensions
          in
          (Module_name.of_string m, ("", Path.relative d (m ^ ext), ty)))
        modules
    in
    Memo.return ms

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld ->
        (Filename.chop_extension (Path.Build.basename mld), mld))
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.Build.to_string_maybe_quoted p1)
          (Path.Build.to_string_maybe_quoted p2)
      ]

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

let check_fallback_artefacts_uniqueness
    (artefacts : (string * Artefact.t list * Lib.t list) list) =
  let ok, _ =
    List.fold_left artefacts ~init:([], [])
      ~f:(fun (ok, artefacts_so_far) (dir, artefacts, libs) ->
        let equal a1 a2 =
          Path.Build.equal (Artefact.html_dir a1) (Artefact.html_dir a2)
        in
        let artefacts =
          List.filter
            ~f:(fun artefact -> not (List.mem artefacts_so_far artefact ~equal))
            artefacts
        in
        ((dir, artefacts, libs) :: ok, artefacts @ artefacts_so_far))
  in
  ok

let fallback_artefacts sctx
    (libs : (Dune_package.Lib.t * Lib.t) Lib_name.Map.t String.Map.t) =
  let ctx = Super_context.context sctx in
  let* public_libs = Scope.DB.public_libs ctx in
  let result =
    String.Map.foldi libs ~init:[] ~f:(fun local_dir libs acc ->
        let lib_names = Lib_name.Map.keys libs in

        let cmti_paths =
          List.map
            ~f:(fun path -> Path.relative path local_dir)
            ctx.findlib_paths
        in
        let result =
          let* mods = Memo.List.map ~f:modules_of_dir cmti_paths in
          let mods = List.flatten mods in
          let mods =
            List.fold_left mods ~init:[]
              ~f:(fun acc (mod_name, (subpath, cmti_file, _)) ->
                let target = Target.ExtLib (local_dir ^ "/" ^ subpath) in
                let artefact =
                  Artefact.make ctx true target
                    (Module
                       ( cmti_file
                       , not
                           (contains_double_underscore
                              (Module_name.to_string mod_name)) ))
                in
                if
                  List.exists
                    ~f:(fun a -> Artefact.(html_file a = html_file artefact))
                    acc
                then acc
                else artefact :: acc)
          in
          let+ libs =
            Memo.List.fold_left ~init:[] lib_names ~f:(fun acc lib_name ->
                let+ lib_opt = Lib.DB.find public_libs lib_name in
                match lib_opt with
                | None -> acc
                | Some l -> l :: acc)
          in
          (local_dir, mods, libs)
        in
        result :: acc)
  in
  let+ result = Memo.all_concurrently result in
  check_fallback_artefacts_uniqueness result

let package_mlds =
  let memo =
    Memo.create "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package)
      (fun (sctx, pkg) ->
        (* CR-someday jeremiedimino: it is weird that we drop the
           [Package.t] and go back to a package name here. Need to try and
           change that one day. *)
        let pkg = Package.name pkg in
        let* mlds = Packages.mlds sctx pkg in
        let mlds = check_mlds_no_dupes ~pkg ~mlds in
        Memo.return (String.Map.remove mlds "index"))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)

let ext_package_mlds (ctx : Context.t) (pkg : Package.Name.t) =
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let* result = Findlib.find_root_package findlib pkg in
  match result with
  | Error _ -> Memo.return []
  | Ok dpkg ->
    let installed = dpkg.files in
    List.filter_map installed ~f:(function
      | Dune_section.Doc, fs ->
        let doc_path = Section.Map.find_exn dpkg.sections Doc in
        Some
          (List.filter_map
             ~f:(fun dst ->
               let str = Install.Dst.to_string dst in
               if Filename.check_suffix str ".mld" then
                 Some (Path.relative doc_path str)
               else None)
             fs)
      | _ -> None)
    |> List.concat |> Memo.return

let singleton_artefacts ctx dwm =
  let info = Lib.info dwm.lib in
  let obj_dir = Lib_info.obj_dir info in
  let target = Target.ExtLib dwm.local_dir in
  let artefacts =
    Modules.fold_no_vlib dwm.modules ~init:[] ~f:(fun m acc ->
        let cmti_file =
          Obj_dir.Module.cmti_file obj_dir ~cm_kind:(Ocaml Cmi) m
        in
        let visible =
          List.mem dwm.entry_modules (Module.name m) ~equal:(fun m1 m2 ->
              Module_name.equal m1 m2)
        in
        let artefact =
          Artefact.make ctx true target (Module (cmti_file, visible))
        in
        artefact :: acc)
  in
  artefacts

let dwm_artefacts ctx dir pkg dwm =
  let t = Target.ExtLib dir in
  let* mlds = ext_package_mlds ctx pkg in
  let index, mlds =
    List.partition ~f:(fun a -> Path.basename a = "index.mld") mlds
  in
  let index =
    match index with
    | [ x ] -> Some x
    | _ -> None
  in
  let artefacts =
    List.map mlds ~f:(fun m -> Artefact.make ctx true t (Mld m))
  in
  let+ arts =
    Memo.List.fold_left ~init:artefacts dwm ~f:(fun acc dwm ->
        let a = singleton_artefacts ctx dwm in
        Memo.return (a @ acc))
  in
  (index, arts)

let pkg_artefacts sctx all pkg =
  let ctx = Super_context.context sctx in
  let+ mlds = Packages.mlds sctx pkg in
  let index, mlds =
    List.partition ~f:(fun a -> Path.Build.basename a = "index.mld") mlds
  in
  let index =
    match index with
    | [ x ] -> Some (Path.build x)
    | _ -> None
  in

  let mlds = check_mlds_no_dupes ~pkg ~mlds in
  let mlds =
    String.Map.values mlds
    |> List.map ~f:(fun mld -> Artefact.make ctx all (Pkg pkg) mld)
  in
  (index, mlds)

let local_lib_artefacts sctx all lib =
  let ctx = Super_context.context sctx in
  let info = Lib.Local.info lib in
  let modes = Lib_info.modes info in
  let mode = Lib_mode.Map.Set.for_merlin modes in
  let cm_kind =
    let open Lib_mode in
    match mode with
    | Ocaml _ -> Cm_kind.Ocaml Cmi
    | Melange -> Cm_kind.Melange Cmi
  in
  let obj_dir = Lib_info.obj_dir info in
  let+ modules = modules_by_lib sctx lib in
  let entry_modules = Modules.entry_modules modules in
  let modules =
    Modules.fold_no_vlib ~init:[]
      ~f:(fun m acc ->
        let visible =
          List.mem entry_modules m ~equal:(fun m1 m2 ->
              Module_name.equal (Module.name m1) (Module.name m2))
        in
        let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind m in
        Artefact.make ctx all (Lib lib) (cmti_file, visible) :: acc)
      modules
  in
  modules

let setup_package_odoc_rules sctx all ~pkg =
  let* mlds = package_mlds sctx ~pkg in
  let ctx = Super_context.context sctx in
  let pkg = Package.name pkg in
  let index = create_index_odoc ctx all (LocalPackage pkg) in
  let* libs = libs_of_pkg ctx ~pkg in
  let artefacts =
    List.map ~f:(Artefact.make ctx all (Pkg pkg)) (String.Map.values mlds)
  in
  let* odocs =
    Memo.parallel_map artefacts ~f:(fun a ->
        compile_mld sctx a ~parent_opt:(Some index)
          ~doc_dir:(Target.odocs_dir ctx all (Pkg pkg))
          ~is_index:false ~children:[])
  in
  let* () =
    link_odoc_rules sctx all ~package:(Some pkg)
      ~libs:(libs :> Lib.t list)
      ~indices:[] artefacts
  in
  let+ () =
    Dep.setup_deps ctx all (Pkg pkg) (Path.set_of_build_paths_list odocs)
  in
  []

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * Lib.Local.t

    let equal (sc1, b1, l1) (sc2, b2, l2) =
      Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2 && b1 = b2

    let hash (sc, b, l) = Poly.hash (Super_context.hash sc, b, Lib.Local.hash l)

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, all, lib) =
    let ctx = Super_context.context sctx in
    let* artefacts = local_lib_artefacts sctx all lib in
    let index = create_index_odoc ctx all (PrivateLib (lib_unique_name lib)) in
    let artefacts =
      List.filter
        ~f:(fun a ->
          match Artefact.artefact_ty a with
          | Module true | Mld -> true
          | _ -> false)
        artefacts
    in
    let* dirs =
      Memo.parallel_map artefacts ~f:(fun a -> html_generate sctx all a)
    in
    let dirs = List.filter_map ~f:(fun x -> x) dirs in
    let html_files =
      List.map
        ~f:(fun a -> Path.build (Artefact.html_file a))
        (index :: artefacts)
    in
    let static_html =
      List.map ~f:(fun b -> Path.build b) (static_html ctx all)
    in
    let* _ = html_generate sctx all index in
    let+ () =
      Rules.Produce.Alias.add_deps
        (Dep.html_alias ctx all (Lib lib))
        (Action_builder.paths (List.rev_append static_html html_files))
    in
    dirs
  in
  Memo.With_implicit_output.create "setup-library-html-rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_lib_html_rules sctx all lib =
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, all, lib)

let setup_pkg_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * string

    let equal (sc1, b1, l1) (sc2, b2, l2) =
      Super_context.equal sc1 sc2 && String.equal l1 l2 && b1 = b2

    let hash (sc, b, l) = Poly.hash (Super_context.hash sc, b, String.hash l)

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, all, pkg_name) =
    let pkg = Package.Name.of_string pkg_name in
    let ctx = Super_context.context sctx in
    let* libs = libs_of_pkg ctx ~pkg in
    let index = create_index_odoc ctx all (LocalPackage pkg) in
    let* _, pkg_artefacts = pkg_artefacts sctx all pkg
    and* lib_artefacts =
      Memo.parallel_map libs ~f:(fun lib -> local_lib_artefacts sctx all lib)
    in
    let artefacts = List.concat (pkg_artefacts :: lib_artefacts) in
    let artefacts =
      List.filter
        ~f:(fun a ->
          match Artefact.artefact_ty a with
          | Mld -> true
          | Module x -> x)
        artefacts
    in
    let html_files =
      List.map
        ~f:(fun a -> Path.build (Artefact.html_file a))
        (index :: artefacts)
    in
    let static_html = List.map ~f:Path.build (static_html ctx all) in
    let* dirs = Memo.List.map artefacts ~f:(html_generate sctx all) in
    let* () =
      Rules.Produce.Alias.add_deps
        (Dep.html_alias ctx all (Pkg pkg))
        (Action_builder.paths (List.rev_append static_html html_files))
    in
    let+ _ = html_generate sctx all index in
    List.filter_map ~f:(fun x -> x) dirs
  in
  Memo.With_implicit_output.create "setup_pkg_html_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_pkg_html_rules sctx all ~pkg =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def
    (sctx, all, Package.Name.to_string pkg)

(* Index generation

   The following are the functions for generating the indexes for
   the four different types of level-1 indexes. There are:

   1. Local packages
   2. Local private libraries
   3. External Dune packages for which we have module info
   4. External opam-lib directories

   These should all be disjoint as local packages override external
   packages, local private libs have unique names generated for them
   and 'other' opam lib directories share the same namespace in the
   opam dir as external packages (as dune enforces that the package
   name is equal to the dir name in the opam lib directory).
*)

let default_index ~pkg_name ~pkg_opt entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s %s}\n"
    (Package.Name.to_string pkg_name)
    (match pkg_opt with
    | None -> "index"
    | Some pkg -> (
      match pkg.Package.synopsis with
      | None -> "index"
      | Some s -> " : " ^ s));

  (match pkg_opt with
  | None -> ()
  | Some pkg -> (
    match pkg.Package.description with
    | None -> ()
    | Some s -> Printf.bprintf b "%s" s));

  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, modules) ->
         Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib);
         Buffer.add_string b
           (match modules with
           | [ x ] ->
             sprintf
               "The entry point of this library is the module:\n{!module-%s}.\n"
               (Module_name.to_string x)
           | _ ->
             sprintf
               "This library exposes the following toplevel modules:\n\
                {!modules:%s}\n"
               (modules
               |> List.sort ~compare:(fun x y -> Module_name.compare x y)
               |> List.map ~f:Module_name.to_string
               |> String.concat ~sep:" ")));
  Buffer.contents b

let default_fallback_index local_path artefacts =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 Index for filesystem path %s}\n" local_path;
  Printf.bprintf b "{!modules:%s}\n"
    (artefacts
    |> List.filter ~f:Artefact.is_visible
    |> List.filter_map ~f:Artefact.module_name
    |> List.sort ~compare:(fun x y -> Module_name.compare x y)
    |> List.map ~f:Module_name.to_string
    |> String.concat ~sep:" ");
  Buffer.contents b

let default_private_index l artefacts =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n"
    (Lib.Local.info l |> Lib_info.name |> Lib_name.to_string);
  let mods =
    List.filter artefacts ~f:Artefact.is_visible
    |> List.filter_map ~f:Artefact.module_name
  in
  Buffer.add_string b
    (match mods with
    | [ x ] ->
      sprintf "The entry point of this library is the module:\n{!module-%s}.\n"
        (Module_name.to_string x)
    | _ ->
      sprintf
        "This library exposes the following toplevel modules:\n{!modules:%s}\n"
        (mods
        |> List.sort ~compare:(fun x y -> Module_name.compare x y)
        |> List.map ~f:(fun m -> Module_name.to_string m)
        |> String.concat ~sep:" "));
  Buffer.contents b

let toplevel_index_contents _sctx indices =
  let set =
    List.fold_left
      ~f:(fun acc dt -> IndexSet.add acc dt)
      ~init:IndexSet.empty indices
  in
  let indices = IndexSet.to_list set in
  let b = Buffer.create 1024 in
  Printf.bprintf b "{0 Docs}\n\n";
  let output_indices label = function
    | [] -> ()
    | indices ->
      Printf.bprintf b "{1 %s}\n" label;
      List.iter
        ~f:(fun i -> Printf.bprintf b "- {!page-\"%s\"}\n" (Index.mld_name i))
        indices
  in
  output_indices "Local packages"
    (List.filter indices ~f:(function
      | Index.LocalPackage _ -> true
      | _ -> false));
  output_indices "Switch-installed packages"
    (List.filter indices ~f:(function
      | Index.ExternalDunePackage _ -> true
      | _ -> false));
  output_indices "Other switch library directories"
    (List.filter indices ~f:(function
      | Index.ExternalFallback _ -> true
      | _ -> false));
  output_indices "Private libraries"
    (List.filter indices ~f:(function
      | Index.PrivateLib _ -> true
      | _ -> false));
  Buffer.contents b

let indexes =
  let run (ctx, all) =
    let* libs_list, packages = Valid.get ctx all in

    let dirs, local_indexes =
      List.fold_left ~init:(String.Set.empty, [])
        ~f:(fun (dirset, indexes) lib ->
          match Lib.Local.of_lib lib with
          | Some local -> (
            match Lib_info.package (Lib.Local.info local) with
            | Some pkg -> (dirset, Index.LocalPackage pkg :: indexes)
            | None ->
              (dirset, Index.PrivateLib (lib_unique_name local) :: indexes))
          | None ->
            let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir in
            let local = Paths.local_path_of_findlib_path ctx obj_dir in
            let top_dir = local |> String.split ~on:'/' |> List.hd in
            (String.Set.add dirset top_dir, indexes))
        libs_list
    in

    let* lib_indexes =
      String.Set.fold dirs ~init:(Memo.return local_indexes) ~f:(fun dir acc ->
          let* acc = acc in
          let+ c = classify_local_dir ctx dir in
          match c with
          | Dune_with_modules (package_name, _dwm) ->
            Index.ExternalDunePackage package_name :: acc
          | Fallback _ -> Index.ExternalFallback (EF dir) :: acc
          | Nothing -> acc)
    in

    let local_pkg_indexes =
      List.map ~f:(fun name -> Index.LocalPackage name) packages
    in

    Memo.return (local_pkg_indexes @ lib_indexes)
  in
  let module Input = struct
    type t = Context.t * bool

    let equal (c1, b1) (c2, b2) = Context.equal c1 c2 && b1 = b2

    let hash (c, b) = Poly.hash (Context.hash c, b)

    let to_dyn _ = Dyn.Opaque
  end in
  Memo.create "main_indexes" ~input:(module Input) run

let get_indexes sctx all =
  let ctx = Super_context.context sctx in
  Memo.exec indexes (ctx, all)

let setup_toplevel_html_rule sctx all =
  let ctx = Super_context.context sctx in
  let artefact = create_index_odoc ctx all Toplevel in
  let* libs_list, pkgs = Valid.get ctx all in
  let deps =
    List.map libs_list ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some local -> (
          match Lib_info.package (Lib.Local.info local) with
          | Some pkg -> Dep.html_alias ctx all (Target.Pkg pkg)
          | None -> Dep.html_alias ctx all (Target.Lib local))
        | None ->
          let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir in
          let local = Paths.local_path_of_findlib_path ctx obj_dir in
          let top_dir = local |> String.split ~on:'/' |> List.hd in
          Dep.html_alias ctx all (Target.ExtLib top_dir))
  in
  let pkgs =
    List.map ~f:(fun p -> Dep.html_alias ctx all (Target.Pkg p)) pkgs
  in
  let deps =
    Dune_engine.Dep.file (Path.build (Artefact.html_file artefact))
    :: List.map ~f:Dune_engine.Dep.alias (deps @ pkgs)
  in
  let deps = Dune_engine.Dep.Set.of_list deps in
  let* () =
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx all (Index Toplevel))
      (Action_builder.deps deps)
  in
  html_generate sctx all artefact

let setup_toplevel_index_rules sctx all =
  let ctx = Super_context.context sctx in

  let* dts = get_indexes sctx all in

  let contents = toplevel_index_contents sctx dts in

  let artefacts = List.map ~f:(fun dt -> create_index_odoc ctx all dt) dts in

  let f = Index.mld_path ctx all Toplevel in
  let mld = create_index_odoc ctx all Toplevel in
  let* () = add_rule sctx (Action_builder.write_file f contents) in
  let* _ =
    compile_mld sctx mld ~doc_dir:(Path.Build.parent_exn f) ~parent_opt:None
      ~is_index:true ~children:artefacts
  in
  let artefact = create_index_odoc ctx all Toplevel in
  let* _ =
    link_odoc_rules sctx all [ artefact ] ~package:None ~libs:[] ~indices:dts
  in
  Memo.return []

type index_content =
  | Symlink of Path.t
  | Generated of string

let general_index_rules sctx all package index index_content children libs =
  let ctx = Super_context.context sctx in
  let index_path = Index.mld_path ctx all index in

  let* () =
    match index_content with
    | Symlink mld ->
      add_rule sctx (Action_builder.symlink ~src:mld ~dst:index_path)
    | Generated content ->
      add_rule sctx (Action_builder.write_file index_path content)
  in

  let mld = create_index_odoc ctx all index in

  let* _ =
    compile_mld sctx mld
      ~doc_dir:(Path.Build.parent_exn index_path)
      ~parent_opt:(Some (create_index_odoc ctx all Toplevel))
      ~is_index:true ~children
  in
  link_odoc_rules sctx all [ mld ] ~package ~libs ~indices:[]

let setup_lnu_index_rules sctx all lnu =
  let ctx = Super_context.context sctx in
  let* lib, lib_db = Scope_key.of_string ctx lnu in
  let* lib =
    let+ lib = Lib.DB.find lib_db lib in
    Option.bind ~f:Lib.Local.of_lib lib
  in
  match lib with
  | None -> Memo.return []
  | Some l ->
    let index = Index.PrivateLib lnu in
    let* artefacts = local_lib_artefacts sctx all l in
    let index_content = Generated (default_private_index l artefacts) in
    let* _ =
      general_index_rules sctx all None index index_content artefacts
        [ (l :> Lib.t) ]
    in
    Memo.return []

let setup_pkg_index_rules sctx all pkg =
  let pkg_name = Package.name pkg in
  let* entry_modules = entry_modules sctx ~pkg:pkg_name in
  let entry_modules =
    Lib.Local.Map.foldi ~init:[] entry_modules ~f:(fun lib modules acc ->
        let info = Lib.Local.info lib in
        let modules =
          modules
          |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
          |> List.map ~f:Module.name
        in
        (Lib_info.name info, modules) :: acc)
  in
  let ctx = Super_context.context sctx in
  let* libs = libs_of_pkg ctx ~pkg:pkg_name in
  let index = Index.LocalPackage pkg_name in
  let* index_path, pkg_artefacts = pkg_artefacts sctx all pkg_name in
  let* lib_artefacts =
    List.map ~f:(fun l -> local_lib_artefacts sctx all l) libs |> Memo.all
  in
  let artefacts = List.concat (pkg_artefacts :: lib_artefacts) in

  let index_content =
    match index_path with
    | Some p -> Symlink p
    | None ->
      Generated (default_index ~pkg_name ~pkg_opt:(Some pkg) entry_modules)
  in
  let* _ =
    general_index_rules sctx all (Some pkg_name) index index_content artefacts
      (libs :> Lib.t list)
  in
  Memo.return []

let setup_external_index_rules sctx dir =
  let ctx = Super_context.context sctx in
  let* c = classify_local_dir ctx dir in
  match c with
  | Nothing -> Memo.return []
  | Fallback f ->
    let index = Index.ExternalFallback (EF dir) in
    (* let index = create_index_odoc ctx index in *)
    let* libs = Valid.filter_fallback_libs ctx true f.libs in
    let* artefacts = fallback_artefacts sctx libs in
    let all_artefacts =
      List.map ~f:(fun (_dir, a, _libs) -> a) artefacts |> List.flatten
    in
    let index_content = Generated (default_fallback_index dir all_artefacts) in
    let libs =
      List.map ~f:(fun (_dir, _a, libs) -> libs) artefacts |> List.flatten
    in
    let* _ =
      general_index_rules sctx true None index index_content all_artefacts libs
    in
    Memo.return []
  | Dune_with_modules (pkg, dwm) ->
    let index = Index.ExternalDunePackage pkg in
    let entry_modules =
      List.fold_left dwm ~init:[] ~f:(fun acc dwm ->
          let info = Lib.info dwm.lib in
          match Lib_info.entry_modules info with
          | External (Ok entry_modules) ->
            (Lib_info.name info, entry_modules) :: acc
          | _ -> acc)
    in
    let* ext_index, artefacts = dwm_artefacts ctx dir pkg dwm in
    let index_content =
      match ext_index with
      | Some p -> Symlink p
      | None ->
        Generated (default_index ~pkg_name:pkg ~pkg_opt:None entry_modules)
    in
    let libs = List.map ~f:(fun dwm -> dwm.lib) dwm in
    let* libs = Valid.filter_libs ctx true libs in
    let* _ = general_index_rules sctx true None index index_content artefacts libs in
    Memo.return []

(* End of index rules *)

(* External rules *)

(* Intra-library module dependencies have to be found out for
   external libraries, we do this by running [odoc compile-deps]
   per module. *)
let external_module_deps_rule sctx all a =
  match Artefact.artefact_ty a with
  | Module _ ->
    let ctx = Super_context.context sctx in
    let* odoc = odoc_program sctx (Paths.root ctx all) in
    let deps_file =
      Path.Build.set_extension (Artefact.odoc_file a) ~ext:".deps"
    in
    let* () =
      Super_context.add_rule sctx ~dir:(Paths.root ctx all)
        (Command.run odoc
           ~dir:(Path.parent_exn (Path.build deps_file))
           ~stdout_to:deps_file
           [ A "compile-deps"; Dep (Artefact.source_file a) ])
    in
    Memo.return (Some deps_file)
  | _ -> Memo.return None

let compile_external_odocs sctx all artefacts parent libs =
  let requires = compile_requires libs in
  let ctx = Super_context.context sctx in
  let* requires =
    Resolve.Memo.bind requires ~f:(fun libs ->
        let+ libs = Valid.filter_libs ctx all libs in
        Resolve.return libs)
  in
  Memo.parallel_iter artefacts ~f:(fun a ->
      let* deps_file = external_module_deps_rule sctx all a in
      match deps_file with
      | None -> Memo.return ()
      | Some deps_file ->
        let module_deps =
          let open Action_builder.O in
          let* l = Action_builder.lines_of (Path.build deps_file) in
          let deps = parse_odoc_deps l in
          let deps' =
            List.filter_map
              ~f:(fun (m', _) ->
                if Artefact.module_name a = Some m' then None
                else
                  match
                    List.find_opt artefacts ~f:(fun a ->
                        Artefact.module_name a = Some m')
                  with
                  | None -> None
                  | Some a' -> Some (Artefact.odoc_file a' |> Path.build))
              deps
          in
          Dune_engine.Dep.Set.of_files deps' |> Action_builder.deps
        in
        let parent_opt =
          match Artefact.artefact_ty a with
          | Module true -> Some parent
          | _ -> None
        in
        let* _odoc_file =
          compile_module sctx all ~artefact:a ~requires ~module_deps
            ~parent_opt ~package:None ~indices:[]
        in
        Memo.return ())

let artefact_rules sctx all artefacts libs parent package aliases =
  let ctx = Super_context.context sctx in
  let* () = compile_external_odocs sctx all artefacts parent libs in
  let* () = link_odoc_rules sctx all artefacts ~package ~libs ~indices:[] in
  let all_deps =
    List.map ~f:(fun a -> Artefact.odoc_file a |> Path.build) artefacts
    |> Path.Set.of_list
  in
  let+ () =
    Memo.List.iter aliases ~f:(fun alias ->
        Dep.setup_deps ctx all alias all_deps)
  in
  []

(* For external fallbacks, we treat all of the artefacts as one big set. We union
   the requires of all of the libs, and remove any self-dependencies.
*)
let fallback_external_rules sctx local_dir fallback =
  if String.contains local_dir '/' then Memo.return []
  else
    let ctx = Super_context.context sctx in
    let parent = create_index_odoc ctx true (ExternalFallback (EF local_dir)) in
    let* libs = Valid.filter_fallback_libs ctx true fallback.libs in
    let* artefacts = fallback_artefacts sctx libs in
    let all_artefacts =
      List.map ~f:(fun (_, a, _) -> a) artefacts |> List.flatten
    in
    let all_libs = List.map ~f:(fun (_, _, l) -> l) artefacts |> List.flatten in
    let aliases =
      List.map artefacts ~f:(fun (dir, _, _) -> Target.ExtLib dir)
    in
    artefact_rules sctx true all_artefacts all_libs parent None aliases

let setup_internal_rules sctx lib_name =
  let ctx = Super_context.context sctx in
  let* lib, lib_db = Scope_key.of_string ctx lib_name in
  let* lib =
    let+ lib = Lib.DB.find lib_db lib in
    Option.bind ~f:Lib.Local.of_lib lib
  in
  match lib with
  | None -> Memo.return []
  | Some local_lib ->
    let info = Lib.Local.info local_lib in
    let package = Lib_info.package info in
    let* artefacts = local_lib_artefacts sctx true local_lib in
    let parent =
      let idx : Index.t =
        match package with
        | Some package -> LocalPackage package
        | None -> PrivateLib (lib_unique_name local_lib)
      in
      create_index_odoc ctx true idx
    in
    let libs = [ (local_lib :> Lib.t) ] in
    artefact_rules sctx true artefacts libs parent None [ Lib local_lib ]

let singleton_external_rules sctx dwm =
  let ctx = Super_context.context sctx in
  let pkg = Lib_name.package_name (Lib.name dwm.lib) in
  let parent = create_index_odoc ctx true (ExternalDunePackage pkg) in
  let local_path =
    let info = Lib.info dwm.lib in
    let obj_dir = info |> Lib_info.obj_dir |> Obj_dir.obj_dir in
    Paths.local_path_of_findlib_path ctx obj_dir
  in
  let target = Target.ExtLib local_path in
  let artefacts = singleton_artefacts ctx dwm in
  artefact_rules sctx true artefacts [ dwm.lib ] parent None [ target ]

let setup_external_rules sctx local_dir =
  let* c = classify_local_dir (Super_context.context sctx) local_dir in
  match c with
  | Nothing -> Memo.return []
  | Dune_with_modules (_package, m) ->
    let* _ =
      List.map m ~f:(fun m -> singleton_external_rules sctx m) |> Memo.all
    in
    Memo.return []
  | Fallback f -> fallback_external_rules sctx local_dir f

let setup_external_html_rules sctx local_dir =
  let ctx = Super_context.context sctx in
  let* c = classify_local_dir ctx local_dir in
  let* artefacts =
    match c with
    | Nothing -> Memo.return None
    | Dune_with_modules (package, ms) ->
      let* valid_ms =
        Memo.List.filter ms ~f:(fun m -> Valid.is_valid ctx true m.lib)
      in
      let artefacts =
        List.map ~f:(singleton_artefacts ctx) valid_ms |> List.concat
      in
      let index = Index.ExternalDunePackage package in
      Memo.return (Some (index, artefacts))
    | Fallback f ->
      let* libs = Valid.filter_fallback_libs ctx true f.libs in
      let+ artefacts = fallback_artefacts sctx libs in
      let artefacts =
        List.map ~f:(fun (_, a, _) -> a) artefacts |> List.flatten
      in
      let index = Index.ExternalFallback (EF local_dir) in
      Some (index, artefacts)
  in
  match artefacts with
  | None -> Memo.return []
  | Some (index, artefacts) ->
    let index_artefact = create_index_odoc ctx true index in
    let artefacts =
      List.filter
        ~f:(fun a ->
          match Artefact.artefact_ty a with
          | Module visible -> visible
          | _ -> true)
        artefacts
    in
    let* dirs =
      Memo.List.map artefacts ~f:(fun a -> html_generate sctx true a)
    in
    let dirs = List.filter_map ~f:(fun x -> x) dirs in
    let html_files =
      List.map (index_artefact :: artefacts) ~f:(fun a ->
          Path.build (Artefact.html_file a))
    in
    let* () =
      Rules.Produce.Alias.add_deps
        (Dep.html_alias ctx true (ExtLib local_dir))
        (Action_builder.paths html_files)
    in
    let+ _ = html_generate sctx true index_artefact in
    dirs

(* End of external rules *)

let gen_project_rules sctx project =
  let* packages = Only_packages.packages_of_project project in
  let ctx = Super_context.context sctx in
  Package.Name.Map_traversals.parallel_iter packages
    ~f:(fun _ (pkg : Package.t) ->
      let dir =
        let pkg_dir = Package.dir pkg in
        Path.Build.append_source ctx.build_dir pkg_dir
      in
      let register alias all =
        let top_alias = Dep.html_alias ctx all (Index Toplevel) in
        Dune_engine.Dep.alias top_alias
        |> Dune_engine.Dep.Set.singleton |> Action_builder.deps
        |> Rules.Produce.Alias.add_deps alias
      in
      let* () = register (Alias.doc ~dir) false in
      register (Alias.doc_all ~dir) true)

let has_rules m =
  let* dirs, rules = Rules.collect (fun () -> m) in
  let directory_targets =
    Path.Build.Map.of_list_exn (List.map ~f:(fun dir -> (dir, Loc.none)) dirs)
  in
  Memo.return
    (Build_config.Rules
       { rules = Memo.return rules
       ; build_dir_only_sub_dirs = Build_config.Rules.Build_only_sub_dirs.empty
       ; directory_targets
       })

let no_rules =
  Build_config.Rules
    { rules = Memo.return Rules.empty
    ; build_dir_only_sub_dirs = Build_config.Rules.Build_only_sub_dirs.empty
    ; directory_targets = Path.Build.Map.empty
    }

(* HTML output for each target is in the same dir. Therefore when creating
   the rules for a particular dir, we need to figure out which target it is.
   This is very similar to the type declared in the module [Target]
   above. *)
type target =
  | Package of Package.t
  | PrivateLib of Lib.Local.t
  | ExtLib
  | Unknown

let with_target sctx dir f =
  let ctx = Super_context.context sctx in
  let* lib, lib_db = Scope_key.of_string ctx dir in
  let* lib =
    let+ lib = Lib.DB.find lib_db lib in
    Option.bind ~f:Lib.Local.of_lib lib
  in
  let* packages = Only_packages.get () in
  match Package.Name.Map.find packages (Package.Name.of_string dir) with
  | Some pkg -> f (Package pkg)
  | None -> (
    match lib with
    | None -> f ExtLib
    | Some lib -> (
      match Lib_info.package (Lib.Local.info lib) with
      | None -> f (PrivateLib lib)
      | Some _ -> f Unknown))

let with_package sctx pkg_name ~f =
  with_target sctx pkg_name (function
    | Package p -> has_rules (f p)
    | _ -> Memo.return no_rules)

let gen_rules sctx ~dir rest =
  let all, rest =
    match rest with
    | "all" :: rest -> (true, rest)
    | _ -> (false, rest)
  in
  match rest with
  | [] ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs =
             Build_config.Rules.Build_only_sub_dirs.singleton ~dir
               Subdir_set.All
         ; directory_targets = Path.Build.Map.empty
         })
  | [ "html" ] ->
    let ctx = Super_context.context sctx in
    let rules =
      let* () = setup_css_rule sctx all in
      let+ _ = setup_toplevel_html_rule sctx all in
      [ Paths.odoc_support ctx all ]
    in
    has_rules rules
  | [ "index" ] -> has_rules (setup_toplevel_index_rules sctx all)
  | [ "index"; "local"; pkg ] ->
    with_package sctx pkg ~f:(fun pkg -> setup_pkg_index_rules sctx all pkg)
  | [ "index"; "private"; lnu ] ->
    has_rules (setup_lnu_index_rules sctx all lnu)
  | [ "index"; "external"; pkg ] ->
    if all then has_rules (setup_external_index_rules sctx pkg)
    else Memo.return (Build_config.Redirect_to_parent Build_config.Rules.empty)
  | [ "odoc"; "pkg"; pkg ] ->
    with_package sctx pkg ~f:(fun pkg ->
        setup_package_odoc_rules sctx all ~pkg)
  | [ "odoc"; "external"; pkg ] -> has_rules (setup_external_rules sctx pkg)
  | [ "odoc"; "internal"; lib ] -> has_rules (setup_internal_rules sctx lib)
  | [ "html"; "docs"; lib_unique_name_or_pkg ] ->
    Log.info [ Pp.textf "html rules called for dir %s" lib_unique_name_or_pkg ];
    with_target sctx lib_unique_name_or_pkg (function
      | Package pkg ->
        Log.info [ Pp.textf "html rules for package %s" lib_unique_name_or_pkg ];
        has_rules (setup_pkg_html_rules sctx all ~pkg:(Package.name pkg))
      | PrivateLib lib -> has_rules (setup_lib_html_rules sctx all lib)
      | ExtLib ->
        has_rules (setup_external_html_rules sctx lib_unique_name_or_pkg)
      | Unknown -> Memo.return no_rules)
  | _ -> Memo.return (Build_config.Redirect_to_parent Build_config.Rules.empty)
