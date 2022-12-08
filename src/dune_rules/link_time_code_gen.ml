open Import

type t =
  { to_link : Lib_flags.Lib_and_module.L.t
  ; force_linkall : bool
  }

let generate_and_compile_module cctx ~precompiled_cmi ~name ~lib ~code ~requires
    =
  let sctx = Compilation_context.super_context cctx in
  let open Memo.O in
  let* module_ =
    let+ modules = Dir_contents.modules_of_lib sctx lib in
    let obj_name =
      Option.map modules ~f:(fun modules ->
          let mli_only = Modules.find modules name |> Option.value_exn in
          Module.obj_name mli_only)
    in
    let src_dir =
      let obj_dir = Compilation_context.obj_dir cctx in
      Obj_dir.obj_dir obj_dir
    in
    Module.generated ?obj_name ~kind:Impl ~src_dir name
  in
  let* () =
    let dir = Compilation_context.dir cctx in
    Super_context.add_rule ~dir sctx
      (let ml =
         Module.file module_ ~ml_kind:Impl
         |> Option.value_exn |> Path.as_in_build_dir_exn
       in
       Action_builder.write_file_dyn ml code)
  in
  let+ () =
    let cctx =
      Compilation_context.for_module_generated_at_link_time cctx ~requires
        ~module_
    in
    Module_compilation.build_module ~precompiled_cmi cctx module_
  in
  Resolve.return module_

let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let prlist buf name l ~f =
  match l with
  | [] -> pr buf "let %s = []" name
  | x :: l ->
    pr buf "let %s =" name;
    Printf.bprintf buf "  [ ";
    f x;
    List.iter l ~f:(fun x ->
        Printf.bprintf buf "  ; ";
        f x);
    pr buf "  ]"

let prvariants buf name preds =
  prlist buf name (Variant.Set.to_list preds) ~f:(fun v ->
      pr buf "%S" (Variant.to_string v))

let sorted_public_lib_names libs =
  List.filter_map libs ~f:(fun lib ->
      let info = Lib.info lib in
      let status = Lib_info.status info in
      if Lib_info.Status.is_private status then None else Some (Lib.name lib))
  |> List.sort ~compare:Lib_name.compare

let findlib_init_code ~preds ~libs =
  let buf = Buffer.create 1024 in
  List.iter (sorted_public_lib_names libs) ~f:(fun lib ->
      pr buf "Findlib.record_package Findlib.Record_core %S;;"
        (Lib_name.to_string lib));
  prvariants buf "preds" preds;
  pr buf
    {ocaml|in
let preds =
  (if Dynlink.is_native then "native" else "byte") :: preds
in
Findlib.record_package_predicates preds;;|ocaml};
  Buffer.contents buf

let build_info_code cctx ~libs ~api_version =
  let open Memo.O in
  (match api_version with
  | Lib_info.Special_builtin_support.Build_info.V1 -> ());
  let placeholder placeholders p =
    Source_tree.nearest_vcs p >>| function
    | None -> ("None", placeholders)
    | Some vcs -> (
      let p =
        Option.value
          (Path.as_in_source_tree vcs.root)
          (* The only VCS root that is potentially not in the source tree is the
             VCS at the root of the repo. For this VCS, it is enough to use the
             source tree root in the placeholder given that we take the nearest
             VCS when performing the actual substitution. *)
          ~default:Path.Source.root
      in
      match Path.Source.Map.find placeholders p with
      | Some var -> (var, placeholders)
      | None ->
        (* [placeholders] is a mapping from source path to variable names. For each
           binding [(p, v)], we will generate the following code:

           {[ let v = Placeholder "%%DUNE_PLACEHOLDER:...:vcs-describe:...:p%%" ]} *)
        let gen_placeholder_var placeholders =
          sprintf "p%d" (Path.Source.Map.cardinal placeholders)
        in
        let var = gen_placeholder_var placeholders in
        let placeholders = Path.Source.Map.set placeholders p var in
        (var, placeholders))
  in
  let version_of_package placeholders (p : Package.t) =
    match p.version with
    | Some v -> Memo.return (sprintf "Some %S" v, placeholders)
    | None -> placeholder placeholders (Package.dir p)
  in
  let* version, placeholders =
    let placeholders = Path.Source.Map.empty in
    match Compilation_context.package cctx with
    | Some p -> version_of_package placeholders p
    | None ->
      let p =
        Path.Build.drop_build_context_exn (Compilation_context.dir cctx)
      in
      placeholder placeholders p
  in
  let+ libs, placeholders =
    Memo.List.fold_left ~init:([], placeholders) libs
      ~f:(fun (libs, placeholders) lib ->
        let+ v, placeholders =
          match Lib_info.version (Lib.info lib) with
          | Some v -> Memo.return (sprintf "Some %S" v, placeholders)
          | None -> (
            match Lib_info.status (Lib.info lib) with
            | Installed_private | Installed -> Memo.return ("None", placeholders)
            | Public (_, p) -> version_of_package placeholders p
            | Private _ ->
              let p =
                Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir
                |> Path.drop_build_context_exn
              in
              placeholder placeholders p)
        in
        ((Lib.name lib, v) :: libs, placeholders))
  in
  let libs = List.rev libs in
  let context = Compilation_context.context cctx in
  let ocaml_version = Ocaml.Version.of_ocaml_config context.ocaml_config in
  let buf = Buffer.create 1024 in
  (* Parse the replacement format described in [artifact_substitution.ml]. *)
  pr buf
    {ocaml|let eval s =
  let s = Bytes.unsafe_to_string (Bytes.unsafe_of_string s) in
  let len = String.length s in
  if s.[0] = '=' then
    let colon_pos = String.index_from s 1 ':' in
    let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
    (* This [min] is because the value might have been truncated
       if it was too large *)
    let vlen = min vlen (len - colon_pos - 1) in
    Some (String.sub s (colon_pos + 1) vlen)
  else
    None
[@@inline never]
|ocaml};
  let fmt_eval : _ format6 =
    if Ocaml.Version.has_sys_opaque_identity ocaml_version then
      "let %s = eval (Sys.opaque_identity %S)"
    else "let %s = eval %S"
  in
  Path.Source.Map.iteri placeholders ~f:(fun path var ->
      pr buf fmt_eval var
        (Artifact_substitution.encode ~min_len:64 (Vcs_describe path)));
  if not (Path.Source.Map.is_empty placeholders) then pr buf "";
  pr buf "let version = %s" version;
  pr buf "";
  prlist buf "statically_linked_libraries" libs ~f:(fun (name, v) ->
      pr buf "%S, %s" (Lib_name.to_string name) v);
  Buffer.contents buf

let dune_site_code () =
  let buf = Buffer.create 5000 in
  pr buf "let hardcoded_ocamlpath = (Sys.opaque_identity %S)"
    (Artifact_substitution.encode ~min_len:4096 Hardcoded_ocaml_path);
  pr buf "let stdlib_dir = (Sys.opaque_identity %S)"
    (Artifact_substitution.encode ~min_len:4096 (Configpath Stdlib));
  Buffer.contents buf

let dune_site_plugins_code ~libs ~builtins =
  let buf = Buffer.create 5000 in
  pr buf
    {ocaml|
let findlib_predicates_set_by_dune pred =
   match Sys.backend_type, pred with
   | Sys.Native, "native" -> true
   | Sys.Bytecode, "byte" -> true|ocaml};
  Variant.Set.iter Findlib.findlib_predicates_set_by_dune ~f:(fun variant ->
      pr buf "   | _, %S -> true" (Variant.to_string variant));
  pr buf "   | _, _ -> false";
  prlist buf "already_linked_libraries" (sorted_public_lib_names libs)
    ~f:(fun lib -> pr buf "%S" (Lib_name.to_string lib));
  pr buf "open Dune_site_plugins.Private_.Meta_parser";
  prlist buf "builtin_library" (Package.Name.Map.to_list builtins)
    ~f:(fun (name, meta) ->
      let meta = Meta.complexify meta in
      let meta =
        Meta.filter_variable
          ~f:(function
            | "plugin" | "directory" | "requires" -> true
            | _ -> false)
          meta
      in
      pr buf "(%S,%s)"
        (Package.Name.to_string name)
        (Dyn.to_string (Meta.to_dyn meta)));
  Buffer.contents buf

let handle_special_libs cctx =
  let ( let& ) m f = Resolve.Memo.bind m ~f in
  let& all_libs = Compilation_context.requires_link cctx in
  let obj_dir = Compilation_context.obj_dir cctx |> Obj_dir.of_local in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let* builtins =
    let+ findlib =
      Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
    in
    Findlib.builtins findlib
  in
  let rec process_libs ~to_link_rev ~force_linkall libs =
    match libs with
    | [] ->
      Resolve.Memo.return { to_link = List.rev to_link_rev; force_linkall }
    | lib :: libs -> (
      match Lib_info.special_builtin_support (Lib.info lib) with
      | None ->
        process_libs libs ~to_link_rev:(Lib lib :: to_link_rev) ~force_linkall
      | Some special -> (
        match special with
        | Build_info { data_module; api_version } ->
          let& module_ =
            generate_and_compile_module cctx ~name:data_module ~lib
              ~code:
                (Action_builder.of_memo
                   (build_info_code cctx ~libs:all_libs ~api_version))
              ~requires:(Resolve.Memo.return [ lib ])
              ~precompiled_cmi:true
          in
          process_libs libs
            ~to_link_rev:(Lib lib :: Module (obj_dir, module_) :: to_link_rev)
            ~force_linkall
        | Findlib_dynload ->
          (* If findlib.dynload is linked, we stores in the binary the packages
             linked by linking just after findlib.dynload a module containing
             the info *)
          let requires =
            (* This shouldn't fail since findlib.dynload depends on dynlink and
               findlib. That's why it's ok to use a dummy location. *)
            let* db = Scope.DB.public_libs ctx in
            let open Resolve.Memo.O in
            let+ dynlink =
              Lib.DB.resolve db (Loc.none, Lib_name.of_string "dynlink")
            and+ findlib =
              Lib.DB.resolve db (Loc.none, Lib_name.of_string "findlib")
            in
            [ dynlink; findlib ]
          in
          let& module_ =
            generate_and_compile_module cctx ~lib
              ~name:(Module_name.of_string "findlib_initl")
              ~code:
                (Action_builder.return
                   (findlib_init_code
                      ~preds:Findlib.findlib_predicates_set_by_dune
                      ~libs:all_libs))
              ~requires ~precompiled_cmi:false
          in
          process_libs libs
            ~to_link_rev:(Module (obj_dir, module_) :: Lib lib :: to_link_rev)
            ~force_linkall:true
        | Configurator _ ->
          (* TODO introduce a runtime dependency or replace with DAP *)
          process_libs libs ~to_link_rev:(Lib lib :: to_link_rev) ~force_linkall
        | Dune_site { data_module; plugins } ->
          let code =
            if plugins then
              Action_builder.return
                (dune_site_plugins_code ~libs:all_libs ~builtins)
            else Action_builder.return (dune_site_code ())
          in
          let& module_ =
            generate_and_compile_module cctx ~name:data_module ~lib ~code
              ~requires:(Resolve.Memo.return [ lib ])
              ~precompiled_cmi:true
          in
          process_libs libs
            ~to_link_rev:(Lib lib :: Module (obj_dir, module_) :: to_link_rev)
            ~force_linkall:true))
  in
  process_libs all_libs ~to_link_rev:[] ~force_linkall:false
