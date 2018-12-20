open! Stdune

(* HACK Otherwise ocamldep doesn't detect this module in bootstrap *)
let () = let module M = Sub_system_info in ()

module Lib = struct
  type 'sub_system t =
    { loc              : Loc.t
    ; name             : Lib_name.t
    ; dir              : Path.t
    ; kind             : Lib_kind.t
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; foreign_objects  : Path.t list
    ; foreign_archives : Path.t list Mode.Dict.t
    ; jsoo_runtime     : Path.t list
    ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
    ; sub_systems      : 'sub_system Sub_system_name.Map.t
    ; virtual_         : bool
    ; implements       : (Loc.t * Lib_name.t) option
    ; modules          : Lib_modules.t option
    ; main_module_name : Module.Name.t option
    ; requires         : (Loc.t * Lib_name.t) list
    ; version          : string option
    ; modes            : Mode.Dict.Set.t
    }

  let make ~loc ~kind ~name ~synopsis ~archives ~plugins ~foreign_objects
        ~foreign_archives ~jsoo_runtime ~main_module_name ~sub_systems
        ~requires ~ppx_runtime_deps ~implements ~virtual_ ~modules ~modes
        ~version ~dir =
    let map_path p = Path.relative dir (Path.basename p) in
    let map_list = List.map ~f:map_path in
    let map_mode = Mode.Dict.map ~f:map_list in
    { loc
    ; kind
    ; name
    ; synopsis
    ; archives = map_mode archives
    ; plugins = map_mode plugins
    ; foreign_objects = map_list foreign_objects
    ; foreign_archives = map_mode foreign_archives
    ; jsoo_runtime = map_list jsoo_runtime
    ; main_module_name
    ; sub_systems
    ; requires
    ; ppx_runtime_deps
    ; implements
    ; version
    ; dir
    ; virtual_
    ; modules
    ; modes
    }

  let dir t = t.dir

  let set_subsystems t sub_systems =
    { t with sub_systems }

  let dir_of_name name =
    let (_, components) = Lib_name.split name in
    Path.Local.L.relative Path.Local.root components

  let encode ~package_root
        { loc = _ ; kind ; synopsis ; name ; archives ; plugins
        ; foreign_objects ; foreign_archives ; jsoo_runtime ; requires
        ; ppx_runtime_deps ; sub_systems ; virtual_
        ; implements ; main_module_name ; version = _; dir = _
        ; modules ; modes
        } =
    let open Dune_lang.Encoder in
    let no_loc f (_loc, x) = f x in
    let path = Path_dune_lang.Local.encode ~dir:package_root in
    let paths name f = field_l name path f in
    let mode_paths name (xs : Path.t Mode.Dict.List.t) =
      field_l name (fun x -> x) (Mode.Dict.List.encode path xs) in
    let libs name = field_l name (no_loc Lib_name.encode) in
    record_fields @@
    [ field "name" Lib_name.encode name
    ; field "kind" Lib_kind.encode kind
    ; field_b "virtual" virtual_
    ; field_o "synopsis" string synopsis
    ; mode_paths "archives" archives
    ; mode_paths "plugins" plugins
    ; paths "foreign_objects" foreign_objects
    ; mode_paths "foreign_archives" foreign_archives
    ; paths "jsoo_runtime" jsoo_runtime
    ; libs "requires" requires
    ; libs "ppx_runtime_deps" ppx_runtime_deps
    ; field_o "implements" (no_loc Lib_name.encode) implements
    ; field_o "main_module_name" Module.Name.encode main_module_name
    ; field_l "modes" (fun x -> x) (Mode.Dict.Set.encode modes)
    ; field_l "modules" (fun x -> x)
        (match modules with
         | None -> []
         | Some modules -> Lib_modules.encode modules)
    ] @ (Sub_system_name.Map.to_list sub_systems
         |> List.map ~f:(fun (name, (_ver, sexps)) ->
           field_l (Sub_system_name.to_string name) (fun x -> x) sexps))

  let decode ~base =
    let open Stanza.Decoder in
    let path = Path_dune_lang.Local.decode ~dir:base in
    let field_l s x = field ~default:[] s (list x) in
    let libs s = field_l s (located Lib_name.decode) in
    let paths s = field_l s path in
    let mode_paths name =
      field ~default:Mode.Dict.List.empty
        name (Mode.Dict.List.decode path) in
    record (
      field_o "main_module_name" Module.Name.decode >>= fun main_module_name ->
      field_o "implements" (located Lib_name.decode) >>= fun implements ->
      let%map synopsis = field_o "synopsis" string
      and loc = loc
      and name = field "name" Lib_name.decode
      and modes = field_l "modes" Mode.decode
      and kind = field "kind" Lib_kind.decode
      and archives = mode_paths "archives"
      and plugins = mode_paths "plugins"
      and foreign_objects = paths "foreign_objects"
      and foreign_archives = mode_paths "foreign_archives"
      and jsoo_runtime = paths "jsoo_runtime"
      and requires = libs "requires"
      and ppx_runtime_deps = libs "ppx_runtime_deps"
      and virtual_ = field_b "virtual"
      and sub_systems = Sub_system_info.record_parser ()
      and modules = field_o "modules" (Lib_modules.decode
                         ~implements:(Option.is_some implements) ~dir:base)
      in
      let modes = Mode.Dict.Set.of_list modes in
      { kind
      ; name
      ; synopsis
      ; loc
      ; archives
      ; plugins
      ; foreign_objects
      ; foreign_archives
      ; jsoo_runtime
      ; requires
      ; ppx_runtime_deps
      ; implements
      ; sub_systems
      ; main_module_name
      ; virtual_
      ; version = None
      ; dir = Path.append_local base (dir_of_name name)
      ; modules
      ; modes
      }
    )

  let name t = t.name
  let version t = t.version
  let kind t = t.kind
  let loc t = t.loc
  let virtual_ t = t.virtual_
  let modules t = t.modules
  let sub_systems t = t.sub_systems
  let synopsis t = t.synopsis
  let main_module_name t = t.main_module_name
  let ppx_runtime_deps t = t.ppx_runtime_deps
  let foreign_objects t = t.foreign_objects
  let archives t = t.archives
  let plugins t = t.plugins
  let jsoo_runtime t = t.jsoo_runtime
  let foreign_archives t = t.foreign_archives
  let requires t = t.requires
  let implements t = t.implements
  let modes t = t.modes

  let compare_name x y = Lib_name.compare x.name y.name
  let wrapped t = Option.map t.modules ~f:Lib_modules.wrapped
end

type 'sub_system t =
  { libs     : 'sub_system Lib.t list
  ; name     : Package.Name.t
  ; version  : string option
  ; dir      : Path.t
  }

let decode ~dir =
  let open Dune_lang.Decoder in
  let%map name = field "name" Package.Name.decode
  and version = field_o "version" string
  and libs = multi_field "library" (Lib.decode ~base:dir)
  in
  { name
  ; version
  ; libs = List.map libs ~f:(fun (lib : _ Lib.t) -> { lib with version })
  ; dir
  }


module Vfile = Versioned_file.Make(struct type t = unit end)

let () = Vfile.Lang.register Stanza.syntax ()

let prepend_version ~dune_version sexps =
  let open Dune_lang.Encoder in
  let list s = Dune_lang.List s in
  [ list [ Dune_lang.atom "lang"
         ; string (Syntax.name Stanza.syntax)
         ; Syntax.Version.encode dune_version
         ]
  ]
  @ sexps

let encode ~dune_version { libs ; name ; version; dir } =
  let list s = Dune_lang.List s in
  let sexp =
    [list [ Dune_lang.atom "name"; Package.Name.encode name ]] in
  let sexp =
    match version with
    | None -> sexp
    | Some version ->
      sexp @ [List [Dune_lang.atom "version"; Dune_lang.atom version]]
  in
  let libs =
    List.map libs ~f:(fun lib ->
      list (Dune_lang.atom "library" :: Lib.encode lib ~package_root:dir))
  in
  prepend_version ~dune_version (sexp @ libs)

module Or_meta = struct
  type nonrec 'sub_system t =
    | Use_meta
    | Dune_package of 'sub_system t

  let encode ~dune_version = function
    | Use_meta ->
      prepend_version ~dune_version [Dune_lang.(List [atom "use_meta"])]
    | Dune_package p -> encode ~dune_version p

  let decode ~dir =
    let open Dune_lang.Decoder in
    (* fields @@ *)
    fields
      (field_b "use_meta" >>= function
       | true -> return Use_meta
       | false -> decode ~dir >>| fun p -> Dune_package p)

  let load p = Vfile.load p ~f:(fun _ -> decode ~dir:(Path.parent_exn p))
end
