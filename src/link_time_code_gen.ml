open Import

module CC = Compilation_context
module SC = Super_context

type t =
  { to_link : Lib.Lib_and_module.t list
  ; force_linkall : bool
  }

let generate_and_compile_module cctx ~precompiled_cmi ~name:basename ~code
      ~requires =
  let open Build.O in
  let sctx       = CC.super_context cctx in
  let obj_dir    = CC.obj_dir       cctx in
  let dir        = CC.dir           cctx in
  let ml = Path.relative (Obj_dir.obj_dir obj_dir) (basename ^ ".ml") in
  SC.add_rule ~dir sctx
    (code >>> Build.write_file_dyn (Path.as_in_build_dir_exn ml));
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
    ~precompiled_cmi
    cctx
    module_;
  module_

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

let findlib_init_code ~preds ~libs =
  let public_libs =
    List.filter
      ~f:(fun lib -> not (Lib_info.Status.is_private (Lib.status lib)))
      libs
  in
  let buf = Buffer.create 1024 in
  List.iter public_libs ~f:(fun lib ->
    pr buf "Findlib.record_package Findlib.Record_core %S;;"
      (Lib_name.to_string (Lib.name lib)));
  prlist buf "preds" (Variant.Set.to_list preds)
    ~f:(fun v -> pr buf "%S" (Variant.to_string v));
  pr buf "in";
  pr buf "let preds =";
  pr buf "  (if Dynlink.is_native then \"native\" else \"byte\") :: preds";
  pr buf "in";
  pr buf "Findlib.record_package_predicates preds;;";
  Buffer.contents buf

let dune_build_info_code cctx ~libs ~package =
  let sctx = CC.super_context cctx in
  let file_tree = Super_context.file_tree sctx in
  (* [placeholders] is a mapping from source path to variable
     names. For each binding [(p, v)], we will generate the following
     code:

     {[
       let v = Placeholder (ref "%%DUNE_PLACEHOLDER:...:vcs-describe:...:p%%")
     ]}
  *)
  let placeholders = ref Path.Source.Map.empty in
  let gen_placeholder_var =
    let n = ref 0 in
    fun () -> let s = sprintf "p%d" !n in incr n; s
  in
  let direct v = sprintf "Direct %S" v in
  let unset = "Unset" in
  let placeholder p =
    match File_tree.nearest_vcs file_tree p with
    | None -> unset
    | Some vcs ->
      let p =
        Option.value (Path.as_in_source_tree vcs.root)
          (* The only VCS root that is potentially not in the source
             tree is the VCS at the root of the repo. For this VCS, it
             is enough to use the source tree root in the placeholder
             given that we take the nearest VCS when performing the
             actual substitution. *)
          ~default:Path.Source.root
      in
      let var =
        match Path.Source.Map.find !placeholders p with
        | Some var -> var
        | None ->
          let  var = gen_placeholder_var () in
          placeholders := Path.Source.Map.add !placeholders p var;
          var
      in
      sprintf "Placeholder %s" var
  in
  let version_of_package (p : Package.t) =
    match p.version with
    | Some (v, _) -> direct v
    | None -> placeholder p.path
  in
  let version =
    match package with
    | Some p -> version_of_package p
    | None ->
      let p = Path.Build.drop_build_context_exn (CC.dir cctx) in
      placeholder p
  in
  let libs =
    List.map libs ~f:(fun lib ->
      Lib.name lib,
      match Lib.version lib with
      | Some v -> direct v
      | None ->
        match Lib.status lib with
        | Installed -> unset
        | Public p -> version_of_package p
        | Private _ ->
          let p = Path.drop_build_context_exn (Obj_dir.dir (Lib.obj_dir lib)) in
          placeholder p)
  in
  let buf = Buffer.create 1024 in
  Path.Source.Map.iteri !placeholders ~f:(fun path var ->
    pr buf "let %s = ref %S"
      var (Artifact_substitution.encode ~min_len:64 (Vcs_describe path)));
  if not (Path.Source.Map.is_empty !placeholders) then pr buf "";
  prlist buf "placeholders" (Path.Source.Map.values !placeholders)
    ~f:(pr buf "%s");
  pr buf "type value =";
  pr buf "  | Unset";
  pr buf "  | Direct of string";
  pr buf "  | Placeholder of string ref";
  pr buf "";
  pr buf "let version = %s" version;
  pr buf "";
  prlist buf "statically_linked_libraries" libs ~f:(fun (name, v) ->
    pr buf "%S, %s" (Lib_name.to_string name) v);
  Buffer.contents buf

let handle_special_libs cctx ~package =
  Result.map (CC.requires_link cctx) ~f:(fun libs ->
    let sctx = CC.super_context cctx in
    let module M = Dune_file.Library.Special_builtin_support.Map in
    let specials = Lib.L.special_builtin_support libs in
    let to_link = Lib.Lib_and_module.L.of_libs libs in
    let to_link =
      match M.find specials Dune_build_info with
      | None -> to_link
      | Some lib ->
        let module_ =
          let name = "dune_build_info_data" in
          generate_and_compile_module
            cctx
            ~name
            ~code:(Build.arr (fun () ->
              dune_build_info_code cctx ~libs ~package ))
            ~requires:(Ok [lib])
            ~precompiled_cmi:true
        in
        Lib.Lib_and_module.Module module_ :: to_link
    in
    if not (M.mem specials Findlib_dynload) then
      { force_linkall = false
      ; to_link
      }
    else begin
      (* If findlib.dynload is linked, we stores in the binary the
         packages linked by linking just after findlib.dynload a
         module containing the info *)
      let requires =
        (* This shouldn't fail since findlib.dynload depends on
           dynlink and findlib. That's why it's ok to use a dummy
           location. *)
        Lib.DB.find_many ~loc:Loc.none (SC.public_libs sctx)
          [ Lib_name.of_string_exn ~loc:None "dynlink"
          ; Lib_name.of_string_exn ~loc:None "findlib"
          ]
      in
      let module_ =
        generate_and_compile_module
          cctx
          ~name:"findlib_initl"
          ~code:(Build.arr (fun () ->
            findlib_init_code ~preds:Findlib.Package.preds ~libs))
          ~requires
          ~precompiled_cmi:false
      in
      let rec insert = function
        | [] -> assert false
        | x :: l ->
          match x with
          | Lib.Lib_and_module.Module _ ->
            x :: insert l
          | Lib lib ->
            match Lib.special_builtin_support lib with
            | Some Findlib_dynload ->
              x :: Module module_ :: l
            | _ -> x :: insert l
      in
      { force_linkall = true
      ; to_link = insert to_link
      }
    end)
