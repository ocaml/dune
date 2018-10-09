open Stdune

type t =
  { dlls : Path.t list
  ; files : Path.t list
  }

let files t = t.files
let dlls t = t.dlls

let all { files; dlls } =
  Path.Set.of_list (List.rev_append dlls files)

module Library = Dune_file.Library

let make ~(ctx : Context.t) ~installable_modules ~dir (lib : Library.t) =
  let (_loc, lib_name_local) = lib.name in
  let obj_dir = Utils.library_object_directory ~dir lib_name_local in
  let ext_obj = ctx.ext_obj in
  let { Mode.Dict.byte; native } =
    Dune_file.Mode_conf.Set.eval lib.modes
      ~has_native:(Option.is_some ctx.ocamlopt)
  in
  let if_ cond l = if cond then l else [] in
  let files =
    let virtual_library = Library.is_virtual lib in
    List.concat
      [ List.concat_map installable_modules ~f:(fun m ->
          List.concat
            [ if_ (Module.is_public m)
                [ Module.cm_file_unsafe m ~obj_dir Cmi ]
            ; if_ (native && Module.has_impl m)
                [ Module.cm_file_unsafe m ~obj_dir Cmx ]
            ; if_ (native && Module.has_impl m && virtual_library)
                [ Module.obj_file m ~obj_dir ~ext:ext_obj ]
            ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~obj_dir)
            ])
      ; if_ (byte && not virtual_library)
          [ Library.archive ~dir lib ~ext:".cma" ]
      ; if virtual_library then (
          (lib.c_names @ lib.cxx_names)
          |> List.map ~f:(fun (_, c) -> Path.relative dir (c ^ ext_obj))
        ) else if Library.has_stubs lib then (
          [ Library.stubs_archive ~dir lib ~ext_lib:ctx.ext_lib ]
        ) else
          []
      ; if_ (native && not virtual_library)
          (let files =
             [ Library.archive ~dir lib ~ext:".cmxa"
             ; Library.archive ~dir lib ~ext:ctx.ext_lib
             ]
           in
           if Dynlink_supported.get lib.dynlink ctx.natdynlink_supported then
             files @ [ Library.archive ~dir lib ~ext:".cmxs" ]
           else
             files)
      ; List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir)
      ; List.map lib.install_c_headers ~f:(fun fn ->
          Path.relative dir (fn ^ ".h"))
      ]
  in
  let dlls  =
    if_ (byte && Library.has_stubs lib &&
         Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries)
      [Library.dll ~dir lib ~ext_dll:ctx.ext_dll]
  in
  { files
  ; dlls
  }
