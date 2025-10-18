open Import
open Dune_lang.Decoder
module Ocaml_flags = Dune_lang.Ocaml_flags

type for_ =
  | Executable
  | Library of Wrapped.t option

type t =
  { loc : Loc.t
  ; modules : Modules_settings.t
  ; empty_module_interface_if_absent : bool
  ; libraries : Lib_dep.t list
  ; foreign_archives : (Loc.t * Foreign.Archive.t) list
  ; extra_objects : Foreign.Objects.t
  ; foreign_stubs : Foreign.Stubs.t list
  ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
  ; preprocessor_deps : Dep_conf.t list
  ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  ; flags : Ocaml_flags.Spec.t
  ; js_of_ocaml : Js_of_ocaml.In_buildable.t Js_of_ocaml.Mode.Pair.t
  ; allow_overlapping_dependencies : bool
  ; ctypes : Ctypes_field.t option
  }

let decode_libraries ~allow_re_export =
  field "libraries" (Lib_dep.L.decode ~allow_re_export) ~default:[]
;;

let decode_preprocess =
  let+ preprocess, preprocessor_deps = Preprocess.preprocess_fields
  and+ instrumentation = Preprocess.Instrumentation.instrumentation in
  let init =
    let f libname = Preprocess.With_instrumentation.Ordinary libname in
    Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
  in
  ( List.fold_left instrumentation ~init ~f:Preprocess.Per_module.add_instrumentation
  , preprocessor_deps )
;;

let decode_ocaml_flags = Ocaml_flags.Spec.decode
let decode_modules = Modules_settings.decode
let decode_lint = field "lint" Lint.decode ~default:Lint.default
let decode_allow_overlapping = field_b "allow_overlapping_dependencies"

let decode (for_ : for_) =
  let use_foreign =
    Dune_lang.Syntax.deleted_in
      Stanza.syntax
      (2, 0)
      ~extra_info:"Use the (foreign_stubs ...) field instead."
  in
  let in_library =
    match for_ with
    | Library _ -> true
    | Executable -> false
  in
  let only_in_library decode = if in_library then decode else return None in
  let add_stubs language ~loc ~names ~flags foreign_stubs =
    match names with
    | None -> foreign_stubs
    | Some names ->
      let names = Ordered_set_lang.replace_standard_with_empty names in
      let flags = Option.value ~default:Ordered_set_lang.Unexpanded.standard flags in
      Foreign.Stubs.make ~loc ~language ~names ~flags :: foreign_stubs
  in
  let+ loc = loc
  and+ preprocess, preprocessor_deps = decode_preprocess
  and+ lint = decode_lint
  and+ foreign_stubs =
    multi_field
      "foreign_stubs"
      (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Foreign.Stubs.decode)
  and+ foreign_archives =
    field_o
      "foreign_archives"
      (Dune_lang.Syntax.since Stanza.syntax (2, 0)
       >>> repeat (located Foreign.Archive.decode))
  and+ extra_objects =
    field
      "extra_objects"
      (Dune_lang.Syntax.since Stanza.syntax (3, 5) >>> Foreign.Objects.decode)
      ~default:Foreign.Objects.empty
  and+ c_flags =
    only_in_library
      (field_o "c_flags" (use_foreign >>> Ordered_set_lang.Unexpanded.decode))
  and+ cxx_flags =
    only_in_library
      (field_o "cxx_flags" (use_foreign >>> Ordered_set_lang.Unexpanded.decode))
  and+ c_names_loc, c_names =
    located
      (only_in_library (field_o "c_names" (use_foreign >>> Ordered_set_lang.decode)))
  and+ cxx_names_loc, cxx_names =
    located
      (only_in_library (field_o "cxx_names" (use_foreign >>> Ordered_set_lang.decode)))
  and+ modules = decode_modules
  and+ self_build_stubs_archive_loc, self_build_stubs_archive =
    located
      (only_in_library
         (field
            ~default:None
            "self_build_stubs_archive"
            (Dune_lang.Syntax.deleted_in
               Stanza.syntax
               (2, 0)
               ~extra_info:"Use the (foreign_archives ...) field instead."
             >>> enter (maybe string))))
  and+ libraries = decode_libraries ~allow_re_export:in_library
  and+ flags = decode_ocaml_flags
  and+ js_of_ocaml =
    field
      "js_of_ocaml"
      (Js_of_ocaml.In_buildable.decode ~in_library ~mode:JS)
      ~default:Js_of_ocaml.In_buildable.default
  and+ wasm_of_ocaml =
    field
      "wasm_of_ocaml"
      (Dune_lang.Syntax.since Stanza.syntax (3, 17)
       >>> Js_of_ocaml.In_buildable.decode ~in_library ~mode:Wasm)
      ~default:Js_of_ocaml.In_buildable.default
  and+ allow_overlapping_dependencies = decode_allow_overlapping
  and+ version = Dune_lang.Syntax.get_exn Stanza.syntax
  and+ ctypes =
    field_o
      "ctypes"
      (Dune_lang.Syntax.since Ctypes_field.syntax (0, 1) >>> Ctypes_field.decode)
  and+ empty_module_interface_if_absent =
    field_b
      "empty_module_interface_if_absent"
      ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
  in
  let foreign_stubs =
    foreign_stubs
    |> add_stubs C ~loc:c_names_loc ~names:c_names ~flags:c_flags
    |> add_stubs Cxx ~loc:cxx_names_loc ~names:cxx_names ~flags:cxx_flags
  in
  let libraries =
    let ctypes_libraries =
      if Option.is_none ctypes
      then []
      else Ctypes_stubs.libraries_needed_for_ctypes ~loc:Loc.none
    in
    libraries @ ctypes_libraries
  in
  let foreign_archives =
    let foreign_archives = Option.value ~default:[] foreign_archives in
    if
      version < (2, 0)
      && List.is_non_empty foreign_stubs
      && Option.is_some self_build_stubs_archive
    then
      User_error.raise
        ~loc:self_build_stubs_archive_loc
        [ Pp.concat
            [ Pp.textf "A library cannot use "
            ; Pp.hbox (Pp.textf "(self_build_stubs_archive ...)")
            ; Pp.textf " and "
            ; Pp.hbox (Pp.textf "(c_names ...)")
            ; Pp.textf " simultaneously. This is supported starting from "
            ; Pp.hbox (Pp.textf "Dune 2.0.")
            ]
        ]
    else (
      match self_build_stubs_archive with
      | None -> foreign_archives
      (* Note: we add "_stubs" to the name, since [self_build_stubs_archive]
         used this naming convention; [foreign_archives] does not use it and
         allows users to name archives as they like (they still need to add
         the "lib" prefix, however, since standard linkers require it). *)
      | Some name -> (loc, Foreign.Archive.stubs name) :: foreign_archives)
  in
  { loc
  ; preprocess
  ; preprocessor_deps
  ; lint
  ; modules
  ; empty_module_interface_if_absent
  ; foreign_stubs
  ; foreign_archives
  ; extra_objects
  ; libraries
  ; flags
  ; js_of_ocaml = { js = js_of_ocaml; wasm = wasm_of_ocaml }
  ; allow_overlapping_dependencies
  ; ctypes
  }
;;

let has_foreign t =
  List.is_non_empty t.foreign_stubs
  || List.is_non_empty t.foreign_archives
  || (not (Foreign.Objects.is_empty t.extra_objects))
  || Option.is_some t.ctypes
;;

let has_foreign_cxx t =
  List.exists
    ~f:(fun stub -> Foreign_language.(equal Cxx stub.Foreign.Stubs.language))
    t.foreign_stubs
;;

let has_mode_dependent_foreign_stubs t =
  List.exists ~f:Foreign.Stubs.is_mode_dependent t.foreign_stubs
;;

let has_foreign_stubs t =
  List.is_non_empty t.foreign_stubs || Ctypes_field.has_stubs t.ctypes
;;
