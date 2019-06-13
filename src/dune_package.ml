open! Stdune

(* HACK Otherwise ocamldep doesn't detect this module in bootstrap *)
let () = let module M = Sub_system_info in ()

module Lib = struct
  type 'sub_system t =
    { loc              : Loc.t
    ; name             : Lib_name.t
    ; obj_dir          : Path.t Obj_dir.t
    ; orig_src_dir     : Path.t option
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
    ; known_implementations :  (Loc.t * Lib_name.t) Variant.Map.t
    ; default_implementation  : (Loc.t * Lib_name.t)  option
    ; implements       : (Loc.t * Lib_name.t) option
    ; modules          : Lib_modules.t option
    ; main_module_name : Module.Name.t option
    ; requires         : (Loc.t * Lib_name.t) list
    ; version          : string option
    ; modes            : Mode.Dict.Set.t
    ; special_builtin_support :
        Dune_file.Library.Special_builtin_support.t option
    }

  let make ~loc ~kind ~name ~synopsis ~archives ~plugins ~foreign_objects
        ~foreign_archives ~jsoo_runtime ~main_module_name ~sub_systems
        ~requires ~ppx_runtime_deps ~implements
        ~default_implementation ~virtual_ ~known_implementations ~modules ~modes
        ~version ~orig_src_dir ~obj_dir
        ~special_builtin_support =
    let dir = Obj_dir.dir obj_dir in
    let map_path p =
      if Path.is_managed p then
        Path.relative dir (Path.basename p)
      else
        p
    in
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
    ; orig_src_dir
    ; virtual_
    ; known_implementations
    ; default_implementation
    ; modules
    ; modes
    ; obj_dir
    ; special_builtin_support
    }

  let obj_dir t = t.obj_dir
  let dir t = Obj_dir.dir t.obj_dir
  let orig_src_dir t = t.orig_src_dir

  let set_subsystems t sub_systems =
    { t with sub_systems }

  let dir_of_name name =
    let (_, components) = Lib_name.split name in
    Path.Local.L.relative Path.Local.root components

  let encode ~package_root
        { loc = _ ; kind ; synopsis ; name ; archives ; plugins
        ; foreign_objects ; foreign_archives ; jsoo_runtime ; requires
        ; ppx_runtime_deps ; sub_systems ; virtual_ ; known_implementations
        ; implements ; default_implementation
        ; main_module_name ; version = _; obj_dir ; orig_src_dir
        ; modules ; modes ; special_builtin_support
        } =
    let open Dune_lang.Encoder in
    let no_loc f (_loc, x) = f x in
    let path = Dpath.Local.encode ~dir:package_root in
    let paths name f = field_l name path f in
    let mode_paths name (xs : Path.t Mode.Dict.List.t) =
      field_l name sexp (Mode.Dict.List.encode path xs) in
    let known_implementations = Variant.Map.to_list known_implementations in
    let libs name = field_l name (no_loc Lib_name.encode) in
    record_fields @@
    [ field "name" Lib_name.encode name
    ; field "kind" Lib_kind.encode kind
    ; field_b "virtual" virtual_
    ; field_o "synopsis" string synopsis
    ; field_o "orig_src_dir" path orig_src_dir
    ; mode_paths "archives" archives
    ; mode_paths "plugins" plugins
    ; paths "foreign_objects" foreign_objects
    ; mode_paths "foreign_archives" foreign_archives
    ; paths "jsoo_runtime" jsoo_runtime
    ; libs "requires" requires
    ; libs "ppx_runtime_deps" ppx_runtime_deps
    ; field_o "implements" (no_loc Lib_name.encode) implements
    ; field_l "known_implementations"
        (pair Variant.encode (no_loc Lib_name.encode)) known_implementations
    ; field_o "default_implementation"
        (no_loc Lib_name.encode) default_implementation
    ; field_o "main_module_name" Module.Name.encode main_module_name
    ; field_l "modes" sexp (Mode.Dict.Set.encode modes)
    ; field_l "obj_dir" sexp (Obj_dir.encode obj_dir)
    ; field_l "modules" sexp
        (match modules with
         | None -> []
         | Some modules -> Lib_modules.encode modules)
    ; field_o "special_builtin_support"
        Dune_file.Library.Special_builtin_support.encode special_builtin_support
    ] @ (Sub_system_name.Map.to_list sub_systems
         |> List.map ~f:(fun (name, (_ver, sexps)) ->
           field_l (Sub_system_name.to_string name) sexp sexps))

  let decode ~base =
    let open Stanza.Decoder in
    let path = Dpath.Local.decode ~dir:base in
    let field_l s x = field ~default:[] s (list x) in
    let libs s = field_l s (located Lib_name.decode) in
    let paths s = field_l s path in
    let mode_paths name =
      field ~default:Mode.Dict.List.empty
        name (Mode.Dict.List.decode path) in
    record (
      let* main_module_name = field_o "main_module_name" Module.Name.decode in
      let* implements = field_o "implements" (located Lib_name.decode) in
      let* default_implementation =
        field_o "default_implementation" (located Lib_name.decode) in
      let* name = field "name" Lib_name.decode in
      let dir = Path.append_local base (dir_of_name name) in
      let* obj_dir = field_o "obj_dir" (Obj_dir.decode ~dir) in
      let obj_dir =
        match obj_dir with
        | None -> Obj_dir.make_external_no_private ~dir
        | Some obj_dir -> obj_dir
      in
      let+ synopsis = field_o "synopsis" string
      and+ loc = loc
      and+ modes = field_l "modes" Mode.decode
      and+ kind = field "kind" Lib_kind.decode
      and+ archives = mode_paths "archives"
      and+ plugins = mode_paths "plugins"
      and+ foreign_objects = paths "foreign_objects"
      and+ foreign_archives = mode_paths "foreign_archives"
      and+ jsoo_runtime = paths "jsoo_runtime"
      and+ requires = libs "requires"
      and+ ppx_runtime_deps = libs "ppx_runtime_deps"
      and+ virtual_ = field_b "virtual"
      and+ known_implementations = field_l "known_implementations"
                                     (pair Variant.decode
                                             (located Lib_name.decode))
      and+ sub_systems = Sub_system_info.record_parser ()
      and+ orig_src_dir = field_o "orig_src_dir" path
      and+ modules =
        let src_dir = Obj_dir.dir obj_dir in
        field_o "modules" (
          Lib_modules.decode ~implements:(Option.is_some implements) ~src_dir)
      and+ special_builtin_support =
        field_o "special_builtin_support"
          (Syntax.since Stanza.syntax (1, 10) >>>
           Dune_file.Library.Special_builtin_support.decode)
      in
      let known_implementations =
        Variant.Map.of_list_exn known_implementations in
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
      ; default_implementation
      ; known_implementations
      ; sub_systems
      ; main_module_name
      ; virtual_
      ; version = None
      ; orig_src_dir
      ; obj_dir
      ; modules
      ; modes
      ; special_builtin_support
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
  let known_implementations t = t.known_implementations
  let default_implementation t = t.default_implementation
  let modes t = t.modes
  let special_builtin_support t = t.special_builtin_support

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
  let+ name = field "name" Package.Name.decode
  and+ version = field_o "version" string
  and+ libs = multi_field "library" (Lib.decode ~base:dir)
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
      sexp @ [ List [ Dune_lang.atom "version"
                    ; Dune_lang.atom_or_quoted_string version
                    ]
             ]
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
    fields
      (let* use_meta = field_b "use_meta" in
       if use_meta then
         return Use_meta
       else
         let+ package = decode ~dir in
         Dune_package package)

  let load p = Vfile.load p ~f:(fun _ -> decode ~dir:(Path.parent_exn p))
end
