open Import

module CC = Compilation_context
module SC = Super_context

let generate_and_compile_module cctx ~name:basename ~code ~requires =
  let sctx       = CC.super_context cctx in
  let obj_dir    = CC.obj_dir       cctx in
  let dir        = CC.dir           cctx in
  let ml = Path.relative (Obj_dir.obj_dir obj_dir) (basename ^ ".ml") in
  SC.add_rule ~dir sctx (Build.write_file ml code);
  let impl = Module.File.make OCaml ml in
  let name = Module.Name.of_string basename in
  let module_ =
    Module.make ~impl name ~visibility:Public ~obj_dir ~kind:Impl
  in
  let opaque =
    Ocaml_version.supports_opaque_for_mli
      (Super_context.context sctx).version
  in
  let cctx =
    Compilation_context.create
      ~super_context:sctx
      ~expander:(Compilation_context.expander cctx)
      ~scope:(Compilation_context.scope cctx)
      ~dir_kind:(Compilation_context.dir_kind cctx)
      ~obj_dir:(Compilation_context.obj_dir cctx)
      ~modules:(Module.Name.Map.singleton name module_)
      ~requires_compile:requires
      ~requires_link:(lazy requires)
      ~flags:Ocaml_flags.empty
      ~opaque
      ()
  in
  Module_compilation.build_module
    ~dep_graphs:(Dep_graph.Ml_kind.dummy module_)
    cctx
    module_;
  module_

let is_findlib_dynload lib =
  match Lib_name.to_string (Lib.name lib) with
  | "findlib.dynload" -> true
  | _ -> false

let libraries_link cctx  =
  match CC.requires_link cctx with
  | Error exn ->
    let arg_spec = Arg_spec.fail exn in
    Staged.stage (fun _mode -> arg_spec)
  | Ok libs ->
    let sctx       = CC.super_context cctx in
    let ctx        = SC.context       sctx in
    let stdlib_dir = ctx.stdlib_dir in
    let has_findlib_dynload =
      List.exists libs ~f:is_findlib_dynload
    in
    if not has_findlib_dynload then
      Staged.stage (fun mode -> Lib.L.link_flags libs ~mode ~stdlib_dir)
    else begin
      (* If findlib.dynload is linked, we stores in the binary the
         packages linked by linking just after findlib.dynload a
         module containing the info *)
      let public_libs =
        List.filter
          ~f:(fun lib -> not (Lib_info.Status.is_private (Lib.status lib)))
          libs
      in
      let requires =
        (* This shouldn't fail since findlib.dynload depends on
           findlib. That's why it's ok to use a dummy location. *)
        Lib.DB.find_many ~loc:Loc.none (SC.public_libs sctx)
          [Lib_name.of_string_exn ~loc:None "findlib"]
      in
      Staged.stage (fun mode ->
        let preds = Variant.Set.add Findlib.Package.preds (Mode.variant mode) in
        let code =
          Format.asprintf "%a@\nFindlib.record_package_predicates %a;;@."
            (Fmt.list ~pp_sep:Fmt.nl (fun fmt lib ->
               Format.fprintf fmt "Findlib.record_package Findlib.Record_core %a;;"
                 Lib_name.pp_quoted (Lib.name lib)))
            public_libs
            (Fmt.ocaml_list Variant.pp)
            (Variant.Set.to_list preds)
        in
        let module_ =
          generate_and_compile_module
            cctx
            ~name:(Format.sprintf "findlib_initl_%s"
                     (Mode.to_string mode))
            ~code
            ~requires
        in
        let rec insert = function
          | [] -> assert false
          | lib :: libs ->
            if is_findlib_dynload lib then
              Lib.Lib_and_module.Lib lib
              :: Module module_
              :: List.map libs ~f:(fun lib -> Lib.Lib_and_module.Lib lib)
            else
              Lib lib :: insert libs
        in
        Arg_spec.S
          [ A "-linkall"
          ; Lib.Lib_and_module.link_flags (insert libs) ~mode ~stdlib_dir
          ])
    end
