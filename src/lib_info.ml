open Stdune

module Status = struct
  type t =
    | Installed
    | Public  of Package.t
    | Private of Dune_project.Name.t

  let pp ppf t =
    Format.pp_print_string ppf
      (match t with
       | Installed -> "installed"
       | Public _ -> "public"
       | Private name ->
         sprintf "private (%s)" (Dune_project.Name.to_string_hum name))

  let is_private = function
    | Private _ -> true
    | Installed | Public _ -> false
end


module Deps = struct
  type t =
    | Simple  of (Loc.t * Lib_name.t) list
    | Complex of Dune_file.Lib_dep.t list

  let of_lib_deps deps =
    let rec loop acc (deps : Dune_file.Lib_dep.t list) =
      match deps with
      | []               -> Some (List.rev acc)
      | Direct x :: deps -> loop (x :: acc) deps
      | Select _ :: _    -> None
    in
    match loop [] deps with
    | Some l -> Simple l
    | None   -> Complex deps

  let to_lib_deps = function
    | Simple  l -> List.map l ~f:Dune_file.Lib_dep.direct
    | Complex l -> l
end

module Source = struct
  type 'a t =
    | Local
    | External of 'a
end

type t =
  { loc              : Loc.t
  ; name             : Lib_name.t
  ; kind             : Lib_kind.t
  ; status           : Status.t
  ; src_dir          : Path.t
  ; orig_src_dir     : Path.t option
  ; obj_dir          : Obj_dir.t
  ; version          : string option
  ; synopsis         : string option
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; foreign_objects  : Path.t list Source.t
  ; foreign_archives : Path.t list Mode.Dict.t
  ; jsoo_runtime     : Path.t list
  ; jsoo_archive     : Path.t option
  ; requires         : Deps.t
  ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
  ; pps              : (Loc.t * Lib_name.t) list
  ; optional         : bool
  ; virtual_deps     : (Loc.t * Lib_name.t) list
  ; dune_version     : Syntax.Version.t option
  ; sub_systems      : Sub_system_info.t Sub_system_name.Map.t
  ; virtual_         : Lib_modules.t Source.t option
  ; implements       : (Loc.t * Lib_name.t) option
  ; variant          : Variant.t option
  ; default_implementation  : (Loc.t * Lib_name.t) option
  ; wrapped          : Wrapped.t Dune_file.Library.Inherited.t option
  ; main_module_name : Dune_file.Library.Main_module_name.t
  ; modes            : Mode.Dict.Set.t
  ; special_builtin_support : Dune_file.Library.Special_builtin_support.t option
  }

let user_written_deps t =
  List.fold_left (t.virtual_deps @ t.ppx_runtime_deps)
    ~init:(Deps.to_lib_deps t.requires)
    ~f:(fun acc s -> Dune_file.Lib_dep.Direct s :: acc)

let of_library_stanza ~dir
      ~lib_config:{ Lib_config.has_native; ext_lib; ext_obj}
      (conf : Dune_file.Library.t) =
  let (_loc, lib_name) = conf.name in
  let obj_dir =
    Obj_dir.make_lib ~dir
      ~has_private_modules:(conf.private_modules <> None) lib_name in
  let gen_archive_file ~dir ext =
    Path.relative dir (Lib_name.Local.to_string lib_name ^ ext) in
  let archive_file = gen_archive_file ~dir in
  let archive_files ~f_ext =
    Mode.Dict.of_func (fun ~mode -> [archive_file (f_ext mode)])
  in
  let jsoo_runtime =
    List.map conf.buildable.js_of_ocaml.javascript_files
      ~f:(Path.relative dir)
  in
  let status =
    match conf.public with
    | None   -> Status.Private (Dune_project.name conf.project)
    | Some p -> Public p.package
  in
  let virtual_library = Dune_file.Library.is_virtual conf in
  let foreign_archives =
    let stubs =
      if Dune_file.Library.has_stubs conf then
        [Dune_file.Library.stubs_archive conf ~dir ~ext_lib]
      else
        []
    in
    { Mode.Dict.
       byte   = stubs
     ; native =
         Path.relative dir (Lib_name.Local.to_string lib_name ^ ext_lib)
         :: stubs
     }
  in
  let foreign_archives =
    match conf.stdlib with
    | Some { exit_module = Some m; _ } ->
      let obj_name = Path.relative dir (Module.Name.uncapitalize m) in
      { Mode.Dict.
        byte =
          Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmo) ::
          foreign_archives.byte
      ; native =
          Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmx) ::
          Path.extend_basename obj_name ~suffix:ext_obj ::
          foreign_archives.native
      }
    | _ -> foreign_archives
  in
  let jsoo_archive =
    Some (gen_archive_file ~dir:(Obj_dir.obj_dir obj_dir) ".cma.js") in
  let virtual_ = Option.map conf.virtual_modules ~f:(fun _ -> Source.Local) in
  let foreign_objects = Source.Local in
  let (archives, plugins) =
    if virtual_library then
      ( Mode.Dict.make_both []
      , Mode.Dict.make_both []
      )
    else
      ( archive_files ~f_ext:Mode.compiled_lib_ext
      , archive_files ~f_ext:Mode.plugin_ext
      )
  in
  let main_module_name = Dune_file.Library.main_module_name conf in
  let name = Dune_file.Library.best_name conf in
  let modes = Dune_file.Mode_conf.Set.eval ~has_native conf.modes in
  { loc = conf.buildable.loc
  ; name
  ; kind     = conf.kind
  ; src_dir  = dir
  ; orig_src_dir = None
  ; obj_dir
  ; version  = None
  ; synopsis = conf.synopsis
  ; archives
  ; plugins
  ; optional = conf.optional
  ; foreign_objects
  ; foreign_archives
  ; jsoo_runtime
  ; jsoo_archive
  ; status
  ; virtual_deps     = conf.virtual_deps
  ; requires         = Deps.of_lib_deps conf.buildable.libraries
  ; ppx_runtime_deps = conf.ppx_runtime_libraries
  ; pps = Dune_file.Preprocess_map.pps conf.buildable.preprocess
  ; sub_systems = conf.sub_systems
  ; dune_version = Some conf.dune_version
  ; virtual_
  ; implements = conf.implements
  ; variant = conf.variant
  ; default_implementation = conf.default_implementation
  ; main_module_name
  ; modes
  ; wrapped = Some conf.wrapped
  ; special_builtin_support = conf.special_builtin_support
  }

let of_dune_lib dp =
  let module Lib = Dune_package.Lib in
  let src_dir = Lib.dir dp in
  let virtual_ =
    if Lib.virtual_ dp then
      Some (Source.External (Option.value_exn (Lib.modules dp)))
    else
      None
  in
  let wrapped =
    Lib.wrapped dp
    |> Option.map ~f:(fun w -> Dune_file.Library.Inherited.This w)
  in
  let obj_dir = Lib.obj_dir dp in
  { loc = Lib.loc dp
  ; name = Lib.name dp
  ; kind = Lib.kind dp
  ; status = Installed
  ; src_dir
  ; orig_src_dir = Lib.orig_src_dir dp
  ; obj_dir
  ; version = Lib.version dp
  ; synopsis = Lib.synopsis dp
  ; requires = Simple (Lib.requires dp)
  ; main_module_name = This (Lib.main_module_name dp)
  ; foreign_objects = Source.External (Lib.foreign_objects dp)
  ; plugins = Lib.plugins dp
  ; archives = Lib.archives dp
  ; ppx_runtime_deps = Lib.ppx_runtime_deps dp
  ; foreign_archives = Lib.foreign_archives dp
  ; jsoo_runtime = Lib.jsoo_runtime dp
  ; jsoo_archive = None
  ; pps = []
  ; optional = false
  ; virtual_deps = []
  ; dune_version = None
  ; sub_systems = Lib.sub_systems dp
  ; virtual_
  ; implements = Lib.implements dp
  ; variant = Lib.variant dp
  ; default_implementation = Lib.default_implementation dp
  ; modes = Lib.modes dp
  ; wrapped
  ; special_builtin_support = Lib.special_builtin_support dp
  }
