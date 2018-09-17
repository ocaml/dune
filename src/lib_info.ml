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

module Virtual = struct
  module Modules = struct
    type t =
      | Unexpanded
  end

  module Dep_graph = struct
    type t =
      | Local
  end

  type t =
    { modules   : Modules.t
    ; dep_graph : Dep_graph.t
    }
end

type t =
  { loc              : Loc.t
  ; kind             : Dune_file.Library.Kind.t
  ; status           : Status.t
  ; src_dir          : Path.t
  ; obj_dir          : Path.t
  ; version          : string option
  ; synopsis         : string option
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; foreign_objects  : string list
  ; foreign_archives : Path.t list Mode.Dict.t
  ; jsoo_runtime     : Path.t list
  ; requires         : Deps.t
  ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
  ; pps              : (Loc.t * Dune_file.Pp.t) list
  ; optional         : bool
  ; virtual_deps     : (Loc.t * Lib_name.t) list
  ; dune_version : Syntax.Version.t option
  ; sub_systems      : Dune_file.Sub_system_info.t Sub_system_name.Map.t
  ; virtual_         : Virtual.t option
  ; implements       : (Loc.t * Lib_name.t) option
  ; main_module_name : Dune_file.Library.Main_module_name.t
  }

let user_written_deps t =
  List.fold_left (t.virtual_deps @ t.ppx_runtime_deps)
    ~init:(Deps.to_lib_deps t.requires)
    ~f:(fun acc s -> Dune_file.Lib_dep.Direct s :: acc)

let of_library_stanza ~dir ~ext_lib (conf : Dune_file.Library.t) =
  let (_loc, lib_name) = conf.name in
  let archive_file ext =
    Path.relative dir (Lib_name.Local.to_string lib_name ^ ext) in
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
  let (foreign_archives, foreign_objects) =
    let stubs =
      if Dune_file.Library.has_stubs conf then
        [Dune_file.Library.stubs_archive conf ~dir ~ext_lib]
      else
        []
    in
    ({ Mode.Dict.
       byte   = stubs
     ; native =
         Path.relative dir (Lib_name.Local.to_string lib_name ^ ext_lib)
         :: stubs
     }
    , List.map (conf.c_names @ conf.cxx_names) ~f:snd
    )
  in
  let virtual_ =
    Option.map conf.virtual_modules ~f:(fun _ ->
      { Virtual.
        modules = Virtual.Modules.Unexpanded
      ; dep_graph = Virtual.Dep_graph.Local
      }
    )
  in
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
  { loc = conf.buildable.loc
  ; kind     = conf.kind
  ; src_dir  = dir
  ; obj_dir  = Utils.library_object_directory ~dir (snd conf.name)
  ; version  = None
  ; synopsis = conf.synopsis
  ; archives
  ; plugins
  ; optional = conf.optional
  ; foreign_objects
  ; foreign_archives
  ; jsoo_runtime
  ; status
  ; virtual_deps     = conf.virtual_deps
  ; requires         = Deps.of_lib_deps conf.buildable.libraries
  ; ppx_runtime_deps = conf.ppx_runtime_libraries
  ; pps = Dune_file.Preprocess_map.pps conf.buildable.preprocess
  ; sub_systems = conf.sub_systems
  ; dune_version = Some conf.dune_version
  ; virtual_
  ; implements = conf.implements
  ; main_module_name
  }

let of_findlib_package pkg =
  let module P = Findlib.Package in
  let loc = Loc.in_file (Path.to_string (P.meta_file pkg)) in
  let add_loc x = (loc, x) in
  let sub_systems =
    match P.dune_file pkg with
    | None -> Sub_system_name.Map.empty
    | Some fn -> Installed_dune_file.load fn
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
  ; foreign_objects  = []
  ; (* We don't know how these are named for external libraries *)
    foreign_archives = Mode.Dict.make_both []
  ; sub_systems      = sub_systems
  ; dune_version = None
  ; virtual_ = None
  ; implements = None
  ; main_module_name = This None
  }
