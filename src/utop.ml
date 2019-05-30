open! Stdune
open Import
open! No_io

let exe_name = "utop"

let utop_dir_basename = ".utop"

let utop_exe =
  (* Use the [.exe] version. As the utop executable is declared with
     [(modes (byte))], the [.exe] correspond the bytecode linked in
     custom mode. We do that so that it works without hassle when
     generating a utop for a library with C stubs. *)
  Filename.concat utop_dir_basename (exe_name ^ Mode.exe_ext Mode.Native)

let source ~dir =
  Toplevel.Source.make
    ~dir:(Path.Build.relative dir utop_dir_basename)
    ~loc:(Loc.in_dir (Path.build dir))
    ~main:"UTop_main.main ();"
    ~name:exe_name

let is_utop_dir dir = Path.Build.basename dir = utop_dir_basename

let libs_under_dir sctx ~db ~dir =
  (let open Option.O in
   let* dir = Path.drop_build_context dir in
   let+ dir = File_tree.find_dir (Super_context.file_tree sctx) dir in
   File_tree.Dir.fold dir ~traverse_ignored_dirs:true
     ~init:[] ~f:(fun dir acc ->
       let dir =
         Path.Build.append_source (Super_context.build_dir sctx)
           (File_tree.Dir.path dir) in
       match Super_context.stanzas_in sctx ~dir with
       | None -> acc
       | Some (d : _ Dir_with_dune.t) ->
         List.fold_left d.data ~init:acc ~f:(fun acc -> function
           | Dune_file.Library l ->
             begin match Lib.DB.find_even_when_hidden db
                           (Dune_file.Library.best_name l) with
             | None -> acc (* library is defined but outside our scope *)
             | Some lib ->
               (* still need to make sure that it's not coming from an external
                  source *)
               let info = Lib.info lib in
               let src_dir = Lib_info.src_dir info in
               if Path.is_descendant ~of_:(Path.build dir) src_dir then
                 lib :: acc
               else
                 acc (* external lib with a name matching our private name *)
             end
           | _ ->
             acc)))
  |> Option.value ~default:[]

let setup sctx ~dir =
  let expander = Super_context.expander sctx ~dir in
  let scope = Super_context.find_scope_by_dir sctx dir in
  let db = Scope.libs scope in
  let libs = libs_under_dir sctx ~db ~dir:(Path.build dir) in
  let source = source ~dir in
  let obj_dir = Toplevel.Source.obj_dir source in
  let loc = Toplevel.Source.loc source in
  let modules = Toplevel.Source.modules source in
  let requires =
    let open Result.O in
    (loc, Lib_name.of_string_exn ~loc:(Some loc) "utop")
    |> Lib.DB.resolve db >>| (fun utop -> utop :: libs)
    >>= Lib.closure ~linking:true
  in
  let cctx =
    Compilation_context.create ()
      ~super_context:sctx
      ~expander
      ~scope
      ~obj_dir
      ~modules
      ~opaque:false
      ~requires_link:(lazy requires)
      ~requires_compile:requires
      ~flags:(Ocaml_flags.append_common
                (Ocaml_flags.default ~profile:(Super_context.profile sctx))
                ["-w"; "-24"])
      ~dynlink:false
      ~package:None
  in
  let toplevel = Toplevel.make ~cctx ~source in
  Toplevel.setup_rules toplevel
