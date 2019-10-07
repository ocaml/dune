open Stdune

module Inherited = struct
  type 'a t =
    | This of 'a
    | From of (Loc.t * Lib_name.t)
end

module Main_module_name = struct
  type t = Module_name.t option Inherited.t
end

module Special_builtin_support = struct
  module Build_info = struct
    type api_version = V1

    let supported_api_versions = [ (1, V1) ]

    type t =
      { data_module : string
      ; api_version : api_version
      }

    let decode =
      let open Dune_lang.Decoder in
      fields
        (let+ data_module = field "data_module" string
         and+ api_version =
           field "api_version"
             (let+ loc = loc
              and+ ver = int in
              match List.assoc supported_api_versions ver with
              | Some x -> x
              | None ->
                User_error.raise ~loc
                  [ Pp.textf
                      "API version %d is not supported. Only the following \
                       versions are currently supported:"
                      ver
                  ; Pp.enumerate supported_api_versions ~f:(fun (n, _) ->
                        Pp.textf "%d" n)
                  ])
         in
         { data_module; api_version })

    let encode { data_module; api_version } =
      let open Dune_lang.Encoder in
      record_fields
        [ field "data_module" string data_module
        ; field "api_version" int
            ( match api_version with
            | V1 -> 1 )
        ]
  end

  type t =
    | Findlib_dynload
    | Build_info of Build_info.t

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ("findlib_dynload", return Findlib_dynload)
      ; ( "build_info"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 11)
          and+ info = Build_info.decode in
          Build_info info )
      ]

  let encode t =
    match t with
    | Findlib_dynload -> Dune_lang.atom "findlib_dynload"
    | Build_info x ->
      Dune_lang.List (Dune_lang.atom "build_info" :: Build_info.encode x)
end

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
    | Public _ ->
      false

  let project_name = function
    | Installed -> None
    | Private project -> Some (Dune_project.name project)
    | Public (name, _) -> Some name
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
  ; requires : Lib_dep.t list
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
  ; wrapped : Wrapped.t Inherited.t option
  ; main_module_name : Main_module_name.t
  ; modes : Mode.Dict.Set.t
  ; special_builtin_support : Special_builtin_support.t option
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

let best_src_dir t = Option.value ~default:t.src_dir t.orig_src_dir

let set_version t version = { t with version }

let for_dune_package t ~ppx_runtime_deps ~requires ~foreign_objects ~obj_dir
    ~implements ~default_implementation ~sub_systems =
  let foreign_objects = Source.External foreign_objects in
  let orig_src_dir =
    match !Clflags.store_orig_src_dir with
    | false -> t.orig_src_dir
    | true ->
      Some
        ( match t.orig_src_dir with
        | Some src_dir -> src_dir
        | None -> (
          match Path.drop_build_context t.src_dir with
          | None -> t.src_dir
          | Some src_dir ->
            Path.source src_dir |> Path.to_absolute_filename |> Path.of_string
          ) )
  in
  { t with
    ppx_runtime_deps
  ; requires
  ; foreign_objects
  ; obj_dir
  ; implements
  ; default_implementation
  ; sub_systems
  ; orig_src_dir
  }

let user_written_deps t =
  List.fold_left (t.virtual_deps @ t.ppx_runtime_deps) ~init:t.requires
    ~f:(fun acc s -> Lib_dep.Direct s :: acc)

let create ~loc ~name ~kind ~status ~src_dir ~orig_src_dir ~obj_dir ~version
    ~synopsis ~main_module_name ~sub_systems ~requires ~foreign_objects
    ~plugins ~archives ~ppx_runtime_deps ~foreign_archives ~jsoo_runtime
    ~jsoo_archive ~pps ~enabled ~virtual_deps ~dune_version ~virtual_
    ~implements ~variant ~known_implementations ~default_implementation ~modes
    ~wrapped ~special_builtin_support =
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

let of_local = map ~f_path:Path.build ~f_obj_dir:Obj_dir.of_local

let as_local_exn =
  map ~f_path:Path.as_in_build_dir_exn ~f_obj_dir:Obj_dir.as_local_exn
