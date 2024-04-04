open Import
open Dune_lang.Decoder

module Wrapped = struct
  include Wrapped

  let default = Simple true

  let make ~wrapped ~implements ~special_builtin_support : t Lib_info.Inherited.t =
    (match wrapped, special_builtin_support with
     | Some (loc, Yes_with_transition _), Some (_loc, _) ->
       (* TODO use _loc *)
       User_error.raise
         ~loc
         [ Pp.text
             "Cannot have transition modules for libraries with special builtin support"
         ]
     | _, _ -> ());
    match wrapped, implements with
    | None, None -> This default
    | None, Some w -> From w
    | Some (_loc, w), None -> This w
    | Some (loc, _), Some _ ->
      User_error.raise
        ~loc
        [ Pp.text
            "Wrapped cannot be set for implementations. It is inherited from the virtual \
             library."
        ]
  ;;

  let field = field_o "wrapped" (located decode)
end

module Modes = struct
  let decode ~stanza_loc ~dune_version project =
    let expected_version = 3, 8 in
    if dune_version >= expected_version
    then Mode_conf.Lib.Set.decode_osl ~stanza_loc project
    else
      (* Old behavior: if old parser succeeds, return that. Otherwise, if
         parsing the ordered set language succeeds, ask the user to upgrade to
         a supported version *)
      try_ Mode_conf.Lib.Set.decode (fun _exn ->
        let+ _modes = Mode_conf.Lib.Set.decode_osl ~stanza_loc project in
        Syntax.Error.since
          stanza_loc
          Stanza.syntax
          expected_version
          ~what:"Ordered set language for modes")
  ;;
end

type visibility =
  | Public of Public_lib.t
  | Private of Package.t option

type t =
  { name : Loc.t * Lib_name.Local.t
  ; visibility : visibility
  ; synopsis : string option
  ; install_c_headers : (Loc.t * string) list
  ; public_headers : Loc.t * Dep_conf.t list
  ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
  ; modes : Mode_conf.Lib.Set.t
  ; kind : Lib_kind.t
  ; library_flags : Ordered_set_lang.Unexpanded.t
  ; c_library_flags : Ordered_set_lang.Unexpanded.t
  ; virtual_deps : (Loc.t * Lib_name.t) list
  ; wrapped : Wrapped.t Lib_info.Inherited.t
  ; optional : bool
  ; buildable : Buildable.t
  ; dynlink : Dynlink_supported.t
  ; project : Dune_project.t
  ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
  ; dune_version : Dune_lang.Syntax.Version.t
  ; virtual_modules : Ordered_set_lang.Unexpanded.t option
  ; implements : (Loc.t * Lib_name.t) option
  ; default_implementation : (Loc.t * Lib_name.t) option
  ; private_modules : Ordered_set_lang.Unexpanded.t option
  ; stdlib : Ocaml_stdlib.t option
  ; special_builtin_support : (Loc.t * Lib_info.Special_builtin_support.t) option
  ; enabled_if : Blang.t
  ; instrumentation_backend : (Loc.t * Lib_name.t) option
  ; melange_runtime_deps : Loc.t * Dep_conf.t list
  }

let no_keep_locs =
  Warning.make ~default:(fun _ -> `Enabled) ~name:"no_keep_locs" ~since:(3, 11)
;;

let decode =
  fields
    (let* stanza_loc = loc in
     let* wrapped = Wrapped.field in
     let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
     let* project = Dune_project.get_exn () in
     let+ buildable = Buildable.decode (Library (Option.map ~f:snd wrapped))
     and+ name = field_o "name" Lib_name.Local.decode_loc
     and+ public = field_o "public_name" (Public_lib.decode ~allow_deprecated_names:false)
     and+ synopsis = field_o "synopsis" string
     and+ install_c_headers =
       field "install_c_headers" (repeat (located string)) ~default:[]
     and+ public_headers =
       field
         "public_headers"
         (Dune_lang.Syntax.since Stanza.syntax (3, 8)
          >>> located (repeat Dep_conf.decode_no_files))
         ~default:(stanza_loc, [])
     and+ ppx_runtime_libraries =
       field "ppx_runtime_libraries" (repeat (located Lib_name.decode)) ~default:[]
     and+ library_flags = Ordered_set_lang.Unexpanded.field "library_flags"
     and+ c_library_flags = Ordered_set_lang.Unexpanded.field "c_library_flags"
     and+ virtual_deps =
       field "virtual_deps" (repeat (located Lib_name.decode)) ~default:[]
     and+ modes =
       field
         "modes"
         (Modes.decode ~stanza_loc ~dune_version project)
         ~default:(Mode_conf.Lib.Set.default stanza_loc)
     and+ kind = field "kind" Lib_kind.decode ~default:Lib_kind.Normal
     and+ optional = field_b "optional"
     and+ no_dynlink = field_b "no_dynlink"
     and+ () =
       let check =
         let* loc = loc in
         Warning_emit.Bag.decode no_keep_locs (fun () ->
           let is_error = dune_version >= (2, 0) in
           let message =
             User_message.make
               ~loc
               [ Pp.text "no_keep_locs is a no-op. Please delete it." ]
           in
           if is_error then raise (User_error.E message) else Memo.return message)
       in
       let+ _ = field_b "no_keep_locs" ~check in
       ()
     and+ sub_systems = Sub_system_info.record_parser
     and+ virtual_modules =
       Ordered_set_lang.Unexpanded.field_o
         ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 7))
         ~since_expanded:Stanza_common.Modules_settings.since_expanded
         "virtual_modules"
     and+ implements =
       field_o
         "implements"
         (Dune_lang.Syntax.since Stanza.syntax (1, 7) >>> located Lib_name.decode)
     and+ default_implementation =
       field_o
         "default_implementation"
         (Dune_lang.Syntax.since Stanza.syntax (2, 6) >>> located Lib_name.decode)
     and+ private_modules =
       Ordered_set_lang.Unexpanded.field_o
         ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 2))
         ~since_expanded:Stanza_common.Modules_settings.since_expanded
         "private_modules"
     and+ stdlib =
       field_o
         "stdlib"
         (Dune_lang.Syntax.since Ocaml_stdlib.syntax (0, 1) >>> Ocaml_stdlib.decode)
     and+ special_builtin_support =
       field_o
         "special_builtin_support"
         (Dune_lang.Syntax.since Stanza.syntax (1, 10)
          >>> located Lib_info.Special_builtin_support.decode)
     and+ enabled_if =
       let open Enabled_if in
       let allowed_vars =
         if Dune_project.dune_version project >= (3, 15)
         then Any
         else
           Only
             (("context_name", (2, 8))
              :: ("profile", (2, 5))
              :: Lib_config.allowed_in_enabled_if)
       in
       decode ~allowed_vars ~since:(Some (1, 10)) ()
     and+ instrumentation_backend =
       field_o
         "instrumentation.backend"
         (Dune_lang.Syntax.since Stanza.syntax (2, 7)
          >>> fields (field "ppx" (located Lib_name.decode)))
     and+ package =
       field_o
         "package"
         (Dune_lang.Syntax.since Stanza.syntax (2, 8) >>> located Stanza_common.Pkg.decode)
     and+ melange_runtime_deps =
       field
         "melange.runtime_deps"
         (Dune_lang.Syntax.since Melange_stanzas.syntax (0, 1)
          >>> located (repeat Dep_conf.decode))
         ~default:(stanza_loc, [])
     in
     let wrapped = Wrapped.make ~wrapped ~implements ~special_builtin_support in
     let name =
       let open Dune_lang.Syntax.Version.Infix in
       match name, public with
       | Some (loc, res), _ -> loc, res
       | None, Some { Public_lib.name = loc, name; _ } ->
         if dune_version >= (1, 1)
         then (
           match Lib_name.to_local (loc, name) with
           | Ok m -> loc, m
           | Error user_message ->
             User_error.raise
               ~loc
               [ Pp.textf "Invalid library name."
               ; Pp.text
                   "Public library names don't have this restriction. You can either \
                    change this public name to be a valid library name or add a \"name\" \
                    field with a valid library name."
               ]
               ~hints:(Lib_name.Local.valid_format_doc :: user_message.hints))
         else
           User_error.raise
             ~loc
             [ Pp.text
                 "name field cannot be omitted before version 1.1 of the dune language"
             ]
       | None, None ->
         User_error.raise
           ~loc:stanza_loc
           [ Pp.text
               (if dune_version >= (1, 1)
                then "supply at least one of name or public_name fields"
                else "name field is missing")
           ]
     in
     let visibility =
       match public, package with
       | None, None -> Private None
       | Some public, None -> Public public
       | None, Some (_loc, package) -> Private (Some package)
       | Some public, Some (loc, _) ->
         User_error.raise
           ~loc
           [ Pp.textf
               "This library has a public_name, it already belongs to the package %s"
               (Package.Name.to_string (Package.name public.package))
           ]
     in
     Option.both virtual_modules implements
     |> Option.iter ~f:(fun (virtual_modules, (_, impl)) ->
       User_error.raise
         ~loc:(Ordered_set_lang.Unexpanded.loc virtual_modules |> Option.value_exn)
         [ Pp.textf
             "A library cannot be both virtual and implement %s"
             (Lib_name.to_string impl)
         ]);
     (match virtual_modules, default_implementation with
      | None, Some (loc, _) ->
        User_error.raise
          ~loc
          [ Pp.text "Only virtual libraries can specify a default implementation." ]
      | _ -> ());
     { name
     ; visibility
     ; synopsis
     ; install_c_headers
     ; public_headers
     ; ppx_runtime_libraries
     ; modes
     ; kind
     ; library_flags
     ; c_library_flags
     ; virtual_deps
     ; wrapped
     ; optional
     ; buildable
     ; dynlink = Dynlink_supported.of_bool (not no_dynlink)
     ; project
     ; sub_systems
     ; dune_version
     ; virtual_modules
     ; implements
     ; default_implementation
     ; private_modules
     ; stdlib
     ; special_builtin_support
     ; enabled_if
     ; instrumentation_backend
     ; melange_runtime_deps
     })
;;

let package t =
  match t.visibility with
  | Public p -> Some p.package
  | Private p -> p
;;

let sub_dir t =
  match t.visibility with
  | Public p -> p.sub_dir
  | Private None -> None
  | Private (Some _) ->
    Lib_name.Local.mangled_path_under_package (snd t.name)
    |> String.concat ~sep:"/"
    |> Option.some
;;

let has_foreign t = Buildable.has_foreign t.buildable
let has_foreign_cxx t = Buildable.has_foreign_cxx t.buildable

let stubs_archive t =
  if Buildable.has_foreign_stubs t.buildable
  then Some (Foreign.Archive.stubs (Lib_name.Local.to_string (snd t.name)))
  else None
;;

let foreign_archives t = List.map ~f:snd t.buildable.foreign_archives

(* This function returns archives files for a given library and mode:
   - For "all" modes it returns:
   - the foreign archives (which are always not mode-dependent)
   - the lib's stubs archive if they are not mode-dependent
   - For a specific mode "m" it returns:
   - the lib's stubs archive for that mode if they are mode-dependent
*)
let foreign_lib_files t ~dir ~ext_lib ~for_mode =
  let stubs_archive = stubs_archive t in
  let foreign_archives = foreign_archives t in
  let stubs_are_mode_dependent = Buildable.has_mode_dependent_foreign_stubs t.buildable in
  let lib_file ~for_mode archive =
    Foreign.Archive.lib_file ~archive ~dir ~ext_lib ~mode:for_mode
  in
  let stubs_archive =
    Option.bind stubs_archive ~f:(fun archive ->
      match stubs_are_mode_dependent, for_mode with
      | false, Mode.Select.All | true, Only _ -> Some (lib_file ~for_mode archive)
      | _ -> None)
  in
  if for_mode = Mode.Select.All
  then (
    let foreign_archives =
      (* Stubs, and thus the lib archives can have mode-dependent versions, but
         right now foreign archives cannot *)
      List.map foreign_archives ~f:(lib_file ~for_mode)
    in
    Option.to_list stubs_archive @ foreign_archives)
  else Option.to_list stubs_archive
;;

let foreign_dll_files t ~dir ~ext_dll =
  let stubs_archive = stubs_archive t in
  let foreign_archives = foreign_archives t in
  let mode =
    if Buildable.has_mode_dependent_foreign_stubs t.buildable
    then
      (* Shared object are never created in Native mode where everything is
         linked statically. *)
      Mode.Select.Only Mode.Byte
    else Mode.Select.All
  in
  let dll_file ~mode archive = Foreign.Archive.dll_file ~archive ~dir ~ext_dll ~mode in
  let foreign_archives = List.map foreign_archives ~f:(dll_file ~mode:Mode.Select.All) in
  (* Stubs can have mode-dependent versions, not foreign archives *)
  match stubs_archive with
  | Some stubs_archive -> dll_file ~mode stubs_archive :: foreign_archives
  | None -> foreign_archives
;;

let archive_basename t ~ext = Lib_name.Local.to_string (snd t.name) ^ ext
let archive t ~dir ~ext = Path.Build.relative dir (archive_basename t ~ext)

let best_name t =
  match t.visibility with
  | Private _ -> Lib_name.of_local t.name
  | Public p -> snd p.name
;;

let is_virtual t = Option.is_some t.virtual_modules
let is_impl t = Option.is_some t.implements

let obj_dir ~dir t =
  let private_lib =
    match t.visibility with
    | Private (Some _) -> true
    | Private None | Public _ -> false
  in
  Obj_dir.make_lib
    ~dir
    ~has_private_modules:
      ((* TODO instead of this fragile approximation, we should be looking at
          [Modules.t] and deciding. Unfortunately, [Obj_dir.t] is currently
          used in some places where [Modules.t] is not yet constructed. *)
       t.private_modules <> None
       || t.buildable.modules.root_module <> None)
    ~private_lib
    (snd t.name)
;;

let main_module_name t : Lib_info.Main_module_name.t =
  match t.implements, t.wrapped with
  | Some x, From _ -> From x
  | Some _, This _ (* cannot specify for wrapped for implements *) | None, From _ ->
    assert false (* cannot inherit for normal libs *)
  | None, This (Simple false) ->
    (match t.stdlib with
     | None -> This None
     | Some _ -> This (Some (Module_name.of_local_lib_name t.name)))
  | None, This (Simple true | Yes_with_transition _) ->
    This (Some (Module_name.of_local_lib_name t.name))
;;

let to_lib_id ~src_dir t =
  let loc, _ = t.name in
  Lib_id.Local.make ~loc ~src_dir (Lib_name.of_local t.name)
;;

let to_lib_info
  conf
  ~expander
  ~dir
  ~lib_config:
    ({ Lib_config.has_native; ext_lib; ext_dll; natdynlink_supported; _ } as lib_config)
  =
  let open Memo.O in
  let obj_dir = obj_dir ~dir conf in
  let archive ?(dir = dir) ext = archive conf ~dir ~ext in
  let modes = Mode_conf.Lib.Set.eval ~has_native conf.modes in
  let archive_for_mode ~f_ext ~mode =
    if Mode.Dict.get modes.ocaml mode then Some (archive (f_ext mode)) else None
  in
  let archives_for_mode ~f_ext =
    Mode.Dict.of_func (fun ~mode -> archive_for_mode ~f_ext ~mode |> Option.to_list)
  in
  let jsoo_runtime =
    List.map conf.buildable.js_of_ocaml.javascript_files ~f:(Path.Build.relative dir)
  in
  let status =
    match conf.visibility with
    | Private pkg -> Lib_info.Status.Private (conf.project, pkg)
    | Public p -> Public (conf.project, p.package)
  in
  let virtual_library = is_virtual conf in
  let foreign_archives =
    let init =
      Mode.Map.Multi.create_for_all_modes
      @@ foreign_lib_files conf ~dir ~ext_lib ~for_mode:All
    in
    Mode.Dict.foldi modes.ocaml ~init ~f:(fun mode enabled acc ->
      if enabled
      then (
        let for_mode = Mode.Select.Only mode in
        let libs = foreign_lib_files conf ~dir ~ext_lib ~for_mode in
        Mode.Map.Multi.add_all acc for_mode libs)
      else acc)
  in
  let native_archives =
    let archive = archive ext_lib in
    if virtual_library || not modes.ocaml.native
    then Lib_info.Files []
    else if Option.is_some conf.implements
            || (Lib_config.linker_can_create_empty_archives lib_config
                && Ocaml.Version.ocamlopt_always_calls_library_linker
                     lib_config.ocaml_version)
    then Lib_info.Files [ archive ]
    else Lib_info.Needs_module_info archive
  in
  let foreign_dll_files = foreign_dll_files conf ~dir ~ext_dll in
  let exit_module = Option.bind conf.stdlib ~f:(fun x -> x.exit_module) in
  let virtual_ = Option.map conf.virtual_modules ~f:(fun _ -> Lib_info.Source.Local) in
  let foreign_objects = Lib_info.Source.Local in
  let archives, plugins =
    if virtual_library
    then Mode.Dict.make_both [], Mode.Dict.make_both []
    else (
      let plugins =
        let archive_file ~mode =
          archive_for_mode ~f_ext:Mode.plugin_ext ~mode |> Option.to_list
        in
        { Mode.Dict.native =
            (if Dynlink_supported.get conf.dynlink natdynlink_supported
             then archive_file ~mode:Native
             else [])
        ; byte = archive_file ~mode:Byte
        }
      in
      archives_for_mode ~f_ext:Mode.compiled_lib_ext, plugins)
  in
  let main_module_name = main_module_name conf in
  let name = best_name conf in
  let lib_id =
    let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
    Lib_id.Local (to_lib_id ~src_dir conf)
  in
  let enabled =
    let+ enabled_if_result =
      let* expander = expander in
      let* project = Expander0.project expander in
      if Dune_project.dune_version project >= (3, 15)
      then Expander0.eval_blang expander conf.enabled_if
      else
        Blang_expand.eval conf.enabled_if ~dir:(Path.build dir) ~f:(fun ~source:_ pform ->
          let+ value =
            match pform with
            | Var Context_name ->
              let context, _ = Path.Build.extract_build_context_exn dir in
              Memo.return context
            | Var Profile ->
              let context, _ = Path.Build.extract_build_context_exn dir in
              let+ profile = Per_context.profile (Context_name.of_string context) in
              Profile.to_string profile
            | _ -> Memo.return @@ Lib_config.get_for_enabled_if lib_config pform
          in
          [ Value.String value ])
    in
    if not enabled_if_result
    then Lib_info.Enabled_status.Disabled_because_of_enabled_if
    else if conf.optional
    then Optional
    else Normal
  in
  let version =
    match status with
    | Public (_, pkg) -> Package.version pkg
    | Installed_private | Installed | Private _ -> None
  in
  let requires = conf.buildable.libraries in
  let loc = conf.buildable.loc in
  let kind = conf.kind in
  let src_dir = dir in
  let orig_src_dir = None in
  let synopsis = conf.synopsis in
  let sub_systems = conf.sub_systems in
  let ppx_runtime_deps = conf.ppx_runtime_libraries in
  let preprocess = conf.buildable.preprocess in
  let virtual_deps = conf.virtual_deps in
  let dune_version = Some conf.dune_version in
  let implements = conf.implements in
  let default_implementation = conf.default_implementation in
  let wrapped = Some conf.wrapped in
  let special_builtin_support = conf.special_builtin_support in
  let instrumentation_backend = conf.instrumentation_backend in
  let entry_modules = Lib_info.Source.Local in
  let melange_runtime_deps =
    let loc, runtime_deps = conf.melange_runtime_deps in
    Lib_info.File_deps.Local (loc, runtime_deps)
  in
  let public_headers =
    let loc, public_headers = conf.public_headers in
    Lib_info.File_deps.Local (loc, public_headers)
  in
  Lib_info.create
    ~loc
    ~path_kind:Local
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
    ~sub_systems
    ~requires
    ~foreign_objects
    ~public_headers
    ~plugins
    ~archives
    ~ppx_runtime_deps
    ~foreign_archives
    ~native_archives
    ~foreign_dll_files
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
    ~modules:Local
    ~wrapped
    ~special_builtin_support
    ~exit_module
    ~instrumentation_backend
    ~melange_runtime_deps
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)
