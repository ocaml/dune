open Import
open! Dune_lang.Decoder

type t =
  { name : Loc.t * Lib_name.Local.t
  ; project : Dune_project.t
  ; visibility : Library.visibility
  ; buildable : Buildable.t
  ; stdlib : Ocaml_stdlib.t option
  ; enabled_if : Blang.t
  ; optional : bool
  ; synopsis : string option
  ; loc : Loc.t
  ; dune_version : Dune_lang.Syntax.Version.t
  }

let to_library t =
  let loc, _ = t.name in
  { Library.name = t.name
  ; visibility = t.visibility
  ; synopsis = t.synopsis
  ; install_c_headers = []
  ; public_headers = loc, []
  ; ppx_runtime_libraries = []
  ; modes = Mode_conf.Lib.Set.of_list [ Ocaml Byte, Inherited ]
  ; kind = Parameter
  ; library_flags = Ordered_set_lang.Unexpanded.standard
  ; c_library_flags = Ordered_set_lang.Unexpanded.standard
  ; virtual_deps = []
  ; wrapped =
      This (Simple true)
      (* We set it as Simple true because, otherwise, we can extract the Singleton main module name. *)
  ; buildable = t.buildable
  ; dynlink = Dynlink_supported.of_bool false
  ; project = t.project
  ; sub_systems = Sub_system_name.Map.empty
  ; dune_version = t.dune_version
  ; virtual_modules = None
  ; implements = None
  ; default_implementation = None
  ; private_modules = None
  ; stdlib = t.stdlib
  ; special_builtin_support = None
  ; enabled_if = t.enabled_if
  ; instrumentation_backend = None
  ; melange_runtime_deps = loc, []
  ; optional = t.optional
  }
;;

let decode =
  fields
    (let* stanza_loc = loc in
     let* project = Dune_project.get_exn () in
     let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
     let+ buildable : Buildable.t =
       let+ loc = loc
       and+ libraries = Buildable.decode_libraries ~allow_re_export:true
       and+ preprocess, preprocessor_deps = Buildable.decode_preprocess
       and+ lint = Buildable.decode_lint
       and+ flags = Buildable.decode_ocaml_flags
       and+ allow_overlapping_dependencies = Buildable.decode_allow_overlapping
       and+ modules = Buildable.decode_modules in
       { Buildable.loc
       ; modules
       ; empty_module_interface_if_absent = false
       ; libraries
       ; foreign_archives = []
       ; extra_objects = Foreign.Objects.empty
       ; foreign_stubs = []
       ; preprocess
       ; preprocessor_deps
       ; lint
       ; flags
       ; js_of_ocaml =
           { js = Js_of_ocaml.In_buildable.default
           ; wasm = Js_of_ocaml.In_buildable.default
           }
       ; allow_overlapping_dependencies
       ; ctypes = None
       }
     and+ name = field_o "name" Lib_name.Local.decode_loc
     and+ public = field_o "public_name" (Public_lib.decode ~allow_deprecated_names:false)
     and+ package = field_o "package" (located Stanza_common.Pkg.decode)
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
     and+ synopsis = field_o "synopsis" string
     and+ optional = field_b "optional" in
     let name =
       match name, public with
       | Some (loc, res), _ -> loc, res
       | None, Some { Public_lib.name = loc, name; _ } ->
         (match Lib_name.to_local (loc, name) with
          | Ok m -> loc, m
          | Error user_message ->
            User_error.raise
              ~loc
              [ Pp.textf "Invalid library_parameter name."
              ; Pp.text
                  "Public library_parameter names don't have this restriction. You can \
                   either change this public name to be a valid library_parameter name \
                   or add a \"name\" field with a valid library_parameter name."
              ]
              ~hints:(Lib_name.Local.valid_format_doc :: user_message.hints))
       | None, None ->
         User_error.raise
           ~loc:stanza_loc
           [ Pp.text "supply at least one of name or public_name fields" ]
     in
     let visibility =
       match public, package with
       | None, None -> Library.Private None
       | Some public, None -> Public public
       | None, Some (_loc, package) -> Private (Some package)
       | Some public, Some (loc, _) ->
         User_error.raise
           ~loc
           [ Pp.textf
               "This library parameter has a public_name, it already belongs to the \
                package %s"
               (Package.Name.to_string (Package.name public.package))
           ]
     in
     to_library
       { name
       ; visibility
       ; buildable
       ; project
       ; stdlib = None
       ; enabled_if
       ; optional
       ; synopsis
       ; loc = stanza_loc
       ; dune_version
       })
;;
