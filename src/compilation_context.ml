open! Stdune
open Import

module SC = Super_context

module Includes = struct
  type t = Command.dynamic Command.t Cm_kind.Dict.t

  let make sctx ~opaque ~requires : _ Cm_kind.Dict.t =
    match requires with
    | Error exn ->
      Cm_kind.Dict.make_all (Command.Fail {fail = fun () -> raise exn})
    | Ok libs ->
      let iflags =
        Lib.L.include_flags_cmd libs ~stdlib_dir:(SC.context sctx).stdlib_dir
      in
      let cmi_includes =
        Command.S [ iflags
                  ; Hidden_deps (Lib_file_deps.deps libs ~groups:[Cmi])
                  ]
      in
      let cmx_includes =
        Command.S
          [ iflags
          ; Hidden_deps
              (if opaque then
                 List.map libs ~f:(fun lib ->
                   (lib, if Lib.is_local lib then
                      [Lib_file_deps.Group.Cmi]
                    else
                      [Cmi; Cmx]))
                 |> Lib_file_deps.deps_with_exts
               else
                 Lib_file_deps.deps libs
                   ~groups:[Lib_file_deps.Group.Cmi; Cmx])
          ]
      in
      { cmi = cmi_includes
      ; cmo = cmi_includes
      ; cmx = cmx_includes
      }

  let empty =
    Cm_kind.Dict.make_all (Command.As [])
end

type t =
  { super_context        : Super_context.t
  ; scope                : Scope.t
  ; expander             : Expander.t
  ; obj_dir              : Obj_dir.t
  ; dir_kind             : Dune_lang.File_syntax.t
  ; modules              : Module.t Module.Name.Map.t
  ; alias_module         : Module.t option
  ; lib_interface_module : Module.t option
  ; flags                : Ocaml_flags.t
  ; requires_compile     : Lib.t list Or_exn.t
  ; requires_link        : Lib.t list Or_exn.t Lazy.t
  ; includes             : Includes.t
  ; preprocessing        : Preprocessing.t
  ; no_keep_locs         : bool
  ; opaque               : bool
  ; stdlib               : Dune_file.Library.Stdlib.t option
  ; vimpl                : Vimpl.t option
  }

let super_context        t = t.super_context
let scope                t = t.scope
let expander             t = t.expander
let dir                  t = Path.as_in_build_dir_exn (Obj_dir.dir t.obj_dir)
let dir_kind             t = t.dir_kind
let obj_dir              t = t.obj_dir
let modules              t = t.modules
let alias_module         t = t.alias_module
let lib_interface_module t = t.lib_interface_module
let flags                t = t.flags
let requires_compile     t = t.requires_compile
let requires_link        t = Lazy.force t.requires_link
let includes             t = t.includes
let preprocessing        t = t.preprocessing
let no_keep_locs         t = t.no_keep_locs
let opaque               t = t.opaque
let stdlib               t = t.stdlib
let vimpl                t = t.vimpl

let context              t = Super_context.context t.super_context

let create ~super_context ~scope ~expander ~obj_dir
      ?vimpl
      ?(dir_kind=Dune_lang.File_syntax.Dune)
      ~modules ?alias_module ?lib_interface_module ~flags
      ~requires_compile ~requires_link
      ?(preprocessing=Preprocessing.dummy) ?(no_keep_locs=false)
      ~opaque ?stdlib () =
  let requires_compile =
    if Dune_project.implicit_transitive_deps (Scope.project scope) then
      Lazy.force requires_link
    else
      requires_compile
  in
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; dir_kind
  ; modules
  ; alias_module
  ; lib_interface_module
  ; flags
  ; requires_compile
  ; requires_link
  ; includes = Includes.make super_context ~opaque ~requires:requires_compile
  ; preprocessing
  ; no_keep_locs
  ; opaque
  ; stdlib
  ; vimpl
  }

let for_alias_module t =
  let flags = Ocaml_flags.default ~profile:(SC.profile t.super_context) in
  { t with
    flags =
      Ocaml_flags.append_common flags
        ["-w"; "-49"; "-nopervasives"; "-nostdlib"]
  ; includes     = Includes.empty
  ; alias_module = None
  ; stdlib       = None
  }

let for_wrapped_compat t modules =
  { t with
    includes = Includes.empty
  ; alias_module = None
  ; stdlib = None
  ; modules
  }
