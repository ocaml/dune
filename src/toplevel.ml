open Stdune

let toplevel_dir_prefix = ".toplevel."

module Source = struct
  type t =
    { name : string
    ; dir : Path.Build.t
    ; loc : Loc.t
    ; main : string
    }

  let main_module t =
    let main_module_name = Module.Name.of_string t.name in
    let src_dir = Path.build t.dir in
    Module.generated ~src_dir main_module_name

  let source_path t =
    Module.file (main_module t) Impl
    |> Option.value_exn
    |> Path.as_in_build_dir_exn

  let obj_dir { dir; name ; _ } =
    Obj_dir.make_exe ~dir ~name

  let modules t =
    let main_module = main_module t in
    let name = Module.name main_module in
    Module.Name.Map.singleton name main_module

  let make ~dir ~loc ~main ~name =
    { dir
    ; main
    ; name
    ; loc
    }

  let of_stanza ~dir ~(toplevel : Dune_file.Toplevel.t) =
    { dir = Path.Build.relative dir (toplevel_dir_prefix ^ toplevel.name)
    ; name = toplevel.name
    ; loc = toplevel.loc
    ; main = "Topmain.main ()"
    }

  let stanza_dir t = Path.Build.parent_exn t.dir

  let program t =
    { Exe.Program.
      loc = t.loc
    ; name = t.name
    ; main_module_name = Module.name (main_module t)
    }

  let pp_ml fmt t ~include_dirs =
    let pp_include fmt =
      let pp_sep fmt () = Format.fprintf fmt "@ ; " in
      Format.pp_print_list ~pp_sep (fun fmt p ->
        Format.fprintf fmt "%S" (Path.to_absolute_filename p)
      ) fmt
    in
    Format.fprintf fmt "@[<v 2>Clflags.include_dirs :=@ [ %a@ ]@];@."
      pp_include include_dirs;
    Format.fprintf fmt "%s@." t.main

  let loc t = t.loc
end

type t =
  { cctx : Compilation_context.t
  ; source : Source.t
  }

let make ~cctx ~source = { cctx ; source }

let setup_module_rules t =
  let dir = Compilation_context.dir t.cctx in
  let sctx = Compilation_context.super_context t.cctx in
  let path = Source.source_path t.source in
  let requires_compile = Compilation_context.requires_compile t.cctx in
  let main_ml =
    let open Build.O in
    Build.of_result_map requires_compile ~f:(fun libs ->
      Build.arr (fun () ->
        let include_dirs =
          let ctx = Super_context.context sctx in
          Path.Set.to_list
            (Lib.L.include_paths libs ~stdlib_dir:ctx.stdlib_dir)
        in
        let b = Buffer.create 64 in
        let fmt = Format.formatter_of_buffer b in
        Source.pp_ml fmt t.source ~include_dirs;
        Format.pp_print_flush fmt ();
        Buffer.contents b))
    >>> Build.write_file_dyn path
  in
  Super_context.add_rule sctx ~dir main_ml

let setup_rules t =
  let linkage = Exe.Linkage.custom in
  let program = Source.program t.source in
  let sctx = Compilation_context.super_context t.cctx in
  Exe.build_and_link t.cctx
    ~program
    ~linkages:[linkage]
    ~link_flags:(Build.return ["-linkall"; "-warn-error"; "-31"]);
  let src = Exe.exe_path t.cctx ~program ~linkage in
  let dir = Source.stanza_dir t.source in
  let dst = Path.Build.relative dir (Path.Build.basename src) in
  Super_context.add_rule sctx ~dir ~loc:t.source.loc
    (Build.symlink ~src:(Path.build src) ~dst);
  setup_module_rules t

module Stanza = struct

  let setup ~sctx ~dir ~(toplevel : Dune_file.Toplevel.t) =
    let source = Source.of_stanza ~dir ~toplevel in
    let expander = Super_context.expander sctx ~dir in
    let scope = Super_context.find_scope_by_dir sctx dir in
    let compile_info =
      let compiler_libs =
        Lib_name.of_string_exn ~loc:(Some source.loc) "compiler-libs.toplevel"
      in
      Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
        [source.loc, source.name]
        (Dune_file.Lib_dep.Direct (source.loc, compiler_libs)
         :: (List.map toplevel.libraries
               ~f:(fun d -> Dune_file.Lib_dep.Direct d)))
        ~pps:[]
        ~allow_overlaps:false
        ~variants:toplevel.variants
    in
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link = Lib.Compile.requires_link compile_info in
    let obj_dir = Source.obj_dir source in
    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~obj_dir
        ~expander
        ~modules:(Source.modules source)
        ~opaque:false
        ~requires_compile
        ~requires_link
        ~flags:(Ocaml_flags.append_common
                  (Ocaml_flags.default ~profile:(Super_context.profile sctx))
                  ["-w"; "-24"])
    in
    let resolved = make ~cctx ~source in
    setup_rules resolved
end
