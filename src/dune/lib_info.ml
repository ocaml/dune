open Stdune

module Status = struct
  type t =
    | Installed
    | Public of Dune_project.Name.t * Package.t
    | Private of Dune_project.t

  let pp ppf t =
    Format.pp_print_string ppf
      ( match t with
      | Installed -> "installed"
      | Public _ -> "public"
      | Private project ->
        let name = Dune_project.name project in
        sprintf "private (%s)" (Dune_project.Name.to_string_hum name) )

  let is_private = function
    | Private _ -> true
    | Installed
     |Public _ ->
      false

  let project_name = function
    | Installed -> None
    | Private project -> Some (Dune_project.name project)
    | Public (name, _) -> Some name
end

module Deps = struct
  type t =
    | Simple of (Loc.t * Lib_name.t) list
    | Complex of Dune_file.Lib_dep.t list

  let of_lib_deps deps =
    let rec loop acc (deps : Dune_file.Lib_dep.t list) =
      match deps with
      | [] -> Some (List.rev acc)
      | Direct x :: deps -> loop (x :: acc) deps
      | Re_export _ :: deps -> loop acc deps
      | Select _ :: _ -> None
    in
    match loop [] deps with
    | Some l -> Simple l
    | None -> Complex deps

  let to_lib_deps = function
    | Simple l -> List.map l ~f:Dune_file.Lib_dep.direct
    | Complex l -> l
end

module Source = struct
  type 'a t =
    | Local
    | External of 'a

  let map t ~f =
    match t with
    | Local -> Local
    | External a -> External (f a)
end

module Enabled_status = struct
  type t =
    | Normal
    | Optional
    | Disabled_because_of_enabled_if
end

type 'path t =
  { loc : Loc.t
  ; name : Lib_name.t
  ; kind : Lib_kind.t
  ; status : Status.t
  ; src_dir : 'path
  ; orig_src_dir : 'path option
  ; obj_dir : 'path Obj_dir.t
  ; version : string option
  ; synopsis : string option
  ; archives : 'path list Mode.Dict.t
  ; plugins : 'path list Mode.Dict.t
  ; foreign_objects : 'path list Source.t
  ; foreign_archives : 'path list Mode.Dict.t  (** [.a/.lib/...] files *)
  ; jsoo_runtime : 'path list
  ; jsoo_archive : 'path option
  ; requires : Deps.t
  ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
  ; pps : (Loc.t * Lib_name.t) list
  ; enabled : Enabled_status.t
  ; virtual_deps : (Loc.t * Lib_name.t) list
  ; dune_version : Dune_lang.Syntax.Version.t option
  ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
  ; virtual_ : Modules.t Source.t option
  ; implements : (Loc.t * Lib_name.t) option
  ; variant : Variant.t option
  ; known_implementations : (Loc.t * Lib_name.t) Variant.Map.t
  ; default_implementation : (Loc.t * Lib_name.t) option
  ; wrapped : Wrapped.t Dune_file.Library.Inherited.t option
  ; main_module_name : Dune_file.Library.Main_module_name.t
  ; modes : Mode.Dict.Set.t
  ; special_builtin_support :
      Dune_file.Library.Special_builtin_support.t option
  ; re_exports : (Loc.t * Lib_name.t) list
  }

let name t = t.name

let version t = t.version

let loc t = t.loc

let requires t = t.requires

let pps t = t.pps

let ppx_runtime_deps t = t.ppx_runtime_deps

let sub_systems t = t.sub_systems

let modes t = t.modes

let archives t = t.archives

let foreign_archives t = t.foreign_archives

let foreign_objects t = t.foreign_objects

let plugins t = t.plugins

let src_dir t = t.src_dir

let variant t = t.variant

let enabled t = t.enabled

let status t = t.status

let kind t = t.kind

let default_implementation t = t.default_implementation

let known_implementations t = t.known_implementations

let obj_dir t = t.obj_dir

let virtual_ t = t.virtual_

let implements t = t.implements

let synopsis t = t.synopsis

let wrapped t = t.wrapped

let special_builtin_support t = t.special_builtin_support

let jsoo_runtime t = t.jsoo_runtime

let jsoo_archive t = t.jsoo_archive

let main_module_name t = t.main_module_name

let orig_src_dir t = t.orig_src_dir

let re_exports t = t.re_exports

let best_src_dir t = Option.value ~default:t.src_dir t.orig_src_dir

let set_version t version = { t with version }

let set_orig_src_dir t orig_src_dir =
  { t with orig_src_dir = Some orig_src_dir }

let set_default_implementation t default_implementation =
  { t with default_implementation }

let set_implements t implements = { t with implements }

let set_ppx_runtime_deps t ppx_runtime_deps = { t with ppx_runtime_deps }

let set_sub_systems t sub_systems = { t with sub_systems }

let set_foreign_objects t foreign_objects =
  { t with foreign_objects = External foreign_objects }

let set_re_exports t re_exports = { t with re_exports }

let user_written_deps t =
  List.fold_left (t.virtual_deps @ t.ppx_runtime_deps)
    ~init:(Deps.to_lib_deps t.requires) ~f:(fun acc s ->
      Dune_file.Lib_dep.Direct s :: acc)

let of_library_stanza ~dir
    ~lib_config:({ Lib_config.has_native; ext_lib; ext_obj; _ } as lib_config)
    ~known_implementations (conf : Dune_file.Library.t) =
  let _loc, lib_name = conf.name in
  let obj_dir = Dune_file.Library.obj_dir ~dir conf in
  let gen_archive_file ~dir ext =
    Path.Build.relative dir (Lib_name.Local.to_string lib_name ^ ext)
  in
  let archive_file = gen_archive_file ~dir in
  let archive_files ~f_ext =
    Mode.Dict.of_func (fun ~mode -> [ archive_file (f_ext mode) ])
  in
  let jsoo_runtime =
    List.map conf.buildable.js_of_ocaml.javascript_files
      ~f:(Path.Build.relative dir)
  in
  let status =
    match conf.public with
    | None -> Status.Private conf.project
    | Some p -> Public (Dune_project.name conf.project, p.package)
  in
  let virtual_library = Dune_file.Library.is_virtual conf in
  let foreign_archives =
    let stubs =
      if Dune_file.Library.has_stubs conf then
        [ Dune_file.Library.stubs_archive conf ~dir ~ext_lib ]
      else
        []
    in
    { Mode.Dict.byte = stubs
    ; native =
        Path.Build.relative dir (Lib_name.Local.to_string lib_name ^ ext_lib)
        :: stubs
    }
  in
  let foreign_archives =
    match conf.stdlib with
    | Some { exit_module = Some m; _ } ->
      let obj_name = Path.Build.relative dir (Module_name.uncapitalize m) in
      { Mode.Dict.byte =
          Path.Build.extend_basename obj_name ~suffix:(Cm_kind.ext Cmo)
          :: foreign_archives.byte
      ; native =
          Path.Build.extend_basename obj_name ~suffix:(Cm_kind.ext Cmx)
          :: Path.Build.extend_basename obj_name ~suffix:ext_obj
          :: foreign_archives.native
      }
    | _ -> foreign_archives
  in
  let jsoo_archive =
    Some (gen_archive_file ~dir:(Obj_dir.obj_dir obj_dir) ".cma.js")
  in
  let virtual_ = Option.map conf.virtual_modules ~f:(fun _ -> Source.Local) in
  let foreign_objects = Source.Local in
  let archives, plugins =
    if virtual_library then
      (Mode.Dict.make_both [], Mode.Dict.make_both [])
    else
      ( archive_files ~f_ext:Mode.compiled_lib_ext
      , archive_files ~f_ext:Mode.plugin_ext )
  in
  let main_module_name = Dune_file.Library.main_module_name conf in
  let name = Dune_file.Library.best_name conf in
  let modes = Dune_file.Mode_conf.Set.eval ~has_native conf.modes in
  let enabled =
    let enabled_if_result =
      Blang.eval conf.enabled_if ~dir:(Path.build dir) ~f:(fun v _ver ->
          match
            (String_with_vars.Var.name v, String_with_vars.Var.payload v)
          with
          | var, None ->
            let value = Lib_config.get_for_enabled_if lib_config ~var in
            Some [ String value ]
          | _ -> None)
    in
    if not enabled_if_result then
      Enabled_status.Disabled_because_of_enabled_if
    else if conf.optional then
      Optional
    else
      Normal
  in
  let version =
    match status with
    | Public (_, pkg) -> pkg.version
    | Installed
     |Private _ ->
      None
  in
  let requires = Deps.of_lib_deps conf.buildable.libraries in
  let re_exports =
    List.filter_map conf.buildable.libraries ~f:(function
      | Re_export l -> Some l
      | _ -> None)
  in
  { loc = conf.buildable.loc
  ; name
  ; kind = conf.kind
  ; src_dir = dir
  ; orig_src_dir = None
  ; obj_dir
  ; version
  ; synopsis = conf.synopsis
  ; archives
  ; plugins
  ; enabled
  ; foreign_objects
  ; foreign_archives
  ; jsoo_runtime
  ; jsoo_archive
  ; status
  ; virtual_deps = conf.virtual_deps
  ; requires
  ; ppx_runtime_deps = conf.ppx_runtime_libraries
  ; pps = Dune_file.Preprocess_map.pps conf.buildable.preprocess
  ; sub_systems = conf.sub_systems
  ; dune_version = Some conf.dune_version
  ; virtual_
  ; implements = conf.implements
  ; variant = conf.variant
  ; known_implementations
  ; default_implementation = conf.default_implementation
  ; main_module_name
  ; modes
  ; wrapped = Some conf.wrapped
  ; special_builtin_support = conf.special_builtin_support
  ; re_exports
  }

let create ~loc ~name ~kind ~status ~src_dir ~orig_src_dir ~obj_dir ~version
    ~synopsis ~main_module_name ~sub_systems ~requires ~foreign_objects
    ~plugins ~archives ~ppx_runtime_deps ~foreign_archives ~jsoo_runtime
    ~jsoo_archive ~pps ~enabled ~virtual_deps ~dune_version ~virtual_
    ~implements ~variant ~known_implementations ~default_implementation ~modes
    ~wrapped ~special_builtin_support ~re_exports =
  { loc
  ; name
  ; kind
  ; status
  ; src_dir
  ; orig_src_dir
  ; obj_dir
  ; version
  ; synopsis
  ; requires
  ; main_module_name
  ; foreign_objects
  ; plugins
  ; archives
  ; ppx_runtime_deps
  ; foreign_archives
  ; jsoo_runtime
  ; jsoo_archive
  ; pps
  ; enabled
  ; virtual_deps
  ; dune_version
  ; sub_systems
  ; virtual_
  ; implements
  ; variant
  ; known_implementations
  ; default_implementation
  ; modes
  ; wrapped
  ; special_builtin_support
  ; re_exports
  }

type external_ = Path.t t

type local = Path.Build.t t

let map t ~f_path ~f_obj_dir =
  let f = f_path in
  let list = List.map ~f in
  let mode_list = Mode.Dict.map ~f:list in
  { t with
    src_dir = f t.src_dir
  ; orig_src_dir = Option.map ~f t.orig_src_dir
  ; obj_dir = f_obj_dir t.obj_dir
  ; archives = mode_list t.archives
  ; plugins = mode_list t.plugins
  ; foreign_objects = Source.map ~f:(List.map ~f) t.foreign_objects
  ; foreign_archives = mode_list t.foreign_archives
  ; jsoo_runtime = List.map ~f t.jsoo_runtime
  ; jsoo_archive = Option.map ~f t.jsoo_archive
  }

let map_path t ~f = map t ~f_path:f ~f_obj_dir:Fn.id

let set_obj_dir t obj_dir = { t with obj_dir }

let of_local = map ~f_path:Path.build ~f_obj_dir:Obj_dir.of_local

let as_local_exn =
  map ~f_path:Path.as_in_build_dir_exn ~f_obj_dir:Obj_dir.as_local_exn
