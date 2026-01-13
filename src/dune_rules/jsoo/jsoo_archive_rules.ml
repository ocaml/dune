open Import
open Memo.O

module Library_key = struct
  type t = Path.Build.t * string

  let equal (dir_a, name_a) (dir_b, name_b) =
    Path.Build.equal dir_a dir_b && String.equal name_a name_b
  ;;

  let hash = Tuple.T2.hash Path.Build.hash String.hash
  let to_dyn (dir, name) = Dyn.Tuple [ Path.Build.to_dyn dir; Dyn.string name ]
end

let find_library ~dir ~lib_name =
  Dune_load.stanzas_in_dir dir
  >>= function
  | None -> Memo.return None
  | Some dune_file ->
    let+ stanzas = Dune_file.stanzas dune_file in
    List.find_map stanzas ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Library.T lib ->
        let name = Lib_name.Local.to_string (snd lib.name) in
        Option.some_if (String.equal name lib_name) lib
      | _ -> None)
;;

let library_cctx_memo =
  Memo.create
    "jsoo-library-cctx"
    ~input:(module Library_key)
    (fun (lib_dir, lib_name) ->
       let* sctx =
         Context.DB.by_dir lib_dir >>| Context.name >>= Super_context.find_exn
       in
       find_library ~dir:lib_dir ~lib_name
       >>= function
       | None -> Memo.return None
       | Some lib ->
         let* dir_contents = Dir_contents.get sctx ~dir:lib_dir in
         let* scope = Scope.DB.find_by_dir lib_dir in
         let* expander = Super_context.expander sctx ~dir:lib_dir in
         (* [compile_context] may produce rules (via [modules_rules]) as implicit
            output. We collect and discard them here; they will be replayed by memo
            when the library directory is loaded through [Lib_rules.rules]. *)
         let* cctx, _cctx_rules =
           Rules.collect (fun () ->
             Lib_rules.compile_context
               lib
               ~sctx
               ~dir_contents
               ~expander
               ~scope
               ~for_:Ocaml)
         in
         let modes = Compilation_context.modes cctx |> Option.value_exn in
         if modes.byte then Memo.return (Some (lib, cctx)) else Memo.return None)
;;

module Lib_archive_rule_key = struct
  type t =
    { lib_dir : Path.Build.t
    ; lib_name : string
    ; config : string
    }

  let equal a b =
    Path.Build.equal a.lib_dir b.lib_dir
    && String.equal a.lib_name b.lib_name
    && String.equal a.config b.config
  ;;

  let hash { lib_dir; lib_name; config } =
    Tuple.T3.hash Path.Build.hash String.hash String.hash (lib_dir, lib_name, config)
  ;;

  let to_dyn { lib_dir; lib_name; config } =
    Dyn.Tuple [ Path.Build.to_dyn lib_dir; Dyn.string lib_name; Dyn.string config ]
  ;;
end

let parse_lib_archive_dir dir =
  let jsoo_dir_config =
    match Path.Build.basename dir with
    | s when Obj_dir.is_jsoo_dirname s -> Some (dir, None)
    | config ->
      (match Path.Build.parent dir with
       | Some parent
         when (not (Path.Build.is_root parent))
              && Obj_dir.is_jsoo_dirname (Path.Build.basename parent) ->
         Some (parent, Some config)
       | _ -> None)
  in
  match jsoo_dir_config with
  | None -> None
  | Some (jsoo_dir, config) ->
    let obj_dir = Path.Build.parent_exn jsoo_dir in
    (* The ".<lib_name>.objs" convention comes from
       [Obj_dir.Paths.library_object_directory]. *)
    let lib_name =
      String.drop_prefix (Path.Build.basename obj_dir) ~prefix:"."
      |> Option.bind ~f:(String.drop_suffix ~suffix:".objs")
    in
    Option.map lib_name ~f:(fun lib_name ->
      let lib_dir = Path.Build.parent_exn obj_dir in
      lib_dir, lib_name, config)
;;

let lib_archive_rules_memo =
  Memo.create
    "jsoo-lib-archive-rules"
    ~input:(module Lib_archive_rule_key)
    (fun { Lib_archive_rule_key.lib_dir; lib_name; config } ->
       Memo.exec library_cctx_memo (lib_dir, lib_name)
       >>= function
       | None -> Memo.return None
       | Some (lib, cctx) ->
         let* sctx =
           Context.DB.by_dir lib_dir >>| Context.name >>= Super_context.find_exn
         in
         let obj_dir = Library.obj_dir ~dir:lib_dir lib in
         let obj_dir_dir = Obj_dir.dir obj_dir in
         let src =
           Library.archive lib ~dir:obj_dir_dir ~ext:(Mode.compiled_lib_ext Mode.Byte)
         in
         let in_context =
           Js_of_ocaml.In_context.make ~dir:lib_dir lib.buildable.js_of_ocaml
         in
         let config = Jsoo_rules.Config.of_string config in
         let+ rules =
           Rules.collect_unit (fun () ->
             Memo.parallel_iter Js_of_ocaml.Mode.all ~f:(fun mode ->
               let in_context = Js_of_ocaml.Mode.Pair.select ~mode in_context in
               Jsoo_rules.build_cm
                 cctx
                 ~dir:obj_dir_dir
                 ~in_context
                 ~mode
                 ~config:(Some config)
                 ~src:(Path.build src)
                 ~deps:(Action_builder.return [])
                 ~obj_dir
               |> Super_context.add_rule sctx ~dir:obj_dir_dir ~loc:lib.buildable.loc))
         in
         Some rules)
;;

type lib_archive_rules =
  | Not_found
  | Root
  | Rules of Rules.t

let lib_archive_rules_for_dir ~dir =
  match parse_lib_archive_dir dir with
  | None -> Memo.return Not_found
  | Some (lib_dir, lib_name, None) ->
    Memo.exec library_cctx_memo (lib_dir, lib_name)
    >>| (function
     | None -> Not_found
     | Some _ -> Root)
  | Some (lib_dir, lib_name, Some config) ->
    Memo.exec lib_archive_rules_memo { lib_dir; lib_name; config }
    >>| (function
     | None -> Not_found
     | Some rules -> Rules rules)
;;
