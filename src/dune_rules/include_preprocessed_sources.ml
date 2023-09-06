open Import
open Memo.O

module Lib_to_preprocess = struct
  type t =
    { name : string
    ; ppx_driver_and_flags : (Path.Build.t * string list) Action_builder.t
    ; sources : Module.t list
    }

  let ppx_driver ~sctx ~scope ~expander lib =
    let* pps = Resolve.Memo.peek (Lib.pps lib) in
    match pps with
    | Ok [] | Error () -> Memo.return None
    | Ok pps ->
      let lib_name =
        match Lib_name.to_local (Loc.none, Lib.name lib) with
        | Ok lib_name -> Some lib_name
        | Error _ -> None
      in
      let pps_list = List.map pps ~f:(fun lib -> Loc.none, Lib.name lib) in
      let ctx = Super_context.context sctx in
      let driver =
        Ppx_driver.get_ppx_driver
          ~loc:Loc.none
          ~expander
          ~scope
          ~lib_name
          ~flags:[]
          ctx
          pps_list
      in
      Memo.return (Some driver)
  ;;

  let sources ~sctx lib =
    let+ modules = Dir_contents.modules_of_lib sctx lib in
    match modules with
    | None -> []
    | Some ms ->
      Modules.fold_user_written ms ~init:[] ~f:(fun module_ acc -> module_ :: acc)
  ;;

  let from_lib ~sctx ~scope ~expander lib =
    let* ppx_driver_and_flags = ppx_driver ~sctx ~scope ~expander lib in
    match ppx_driver_and_flags with
    | Some ppx_driver_and_flags ->
      let+ sources = sources ~sctx lib in
      let name = Lib_name.to_string (Lib.name lib) in
      Some { name; ppx_driver_and_flags; sources }
    | None -> Memo.return None
  ;;
end

module Libs_and_exes = Monoid.Appendable_list (struct
    type t = Lib.t
  end)

module Source_tree_map_reduce = Source_tree.Dir.Make_map_reduce (Memo) (Libs_and_exes)

let libs_or_exes_under_dir ~sctx ~scope dir =
  let db = Scope.libs scope in
  (match Path.drop_build_context dir with
   | None -> Memo.return None
   | Some dir -> Source_tree.find_dir dir)
  >>= function
  | None -> Memo.return []
  | Some dir ->
    let+ libs =
      Source_tree_map_reduce.map_reduce
        dir
        ~traverse:Source_dir_status.Set.all
        ~f:(fun dir ->
          let build_dir = Context.build_dir (Super_context.context sctx) in
          let dir = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
          Dune_load.stanzas_in_dir dir
          >>= function
          | None -> Memo.return Libs_and_exes.empty
          | Some (d : Dune_file.t) ->
            let* stanzas = Dune_file.stanzas d in
            Memo.List.fold_left stanzas ~init:Libs_and_exes.empty ~f:(fun acc stanza ->
              match Stanza.repr stanza with
              | Library.T l ->
                let+ lib =
                  let open Memo.O in
                  let+ resolve =
                    Lib.DB.resolve_when_exists db (l.buildable.loc, Library.best_name l)
                  in
                  Option.map resolve ~f:Resolve.peek
                  (* external lib with a name matching our private name *)
                in
                (match lib with
                 | None | Some (Error ()) ->
                   (* library is defined but outside our scope or is disabled *)
                   acc
                 | Some (Ok lib) ->
                   (* still need to make sure that it's not coming from an external
                      source *)
                   let info = Lib.info lib in
                   let src_dir = Lib_info.src_dir info in
                   (* Only select libraries that are not implementations.
                      Implementations are selected using the default implementation
                      feature. *)
                   let not_impl = Option.is_none (Lib_info.implements info) in
                   if not_impl && Path.is_descendant ~of_:(Path.build dir) src_dir
                   then Appendable_list.cons lib acc
                   else acc)
              | Executables.T exes ->
                let+ libs =
                  let open Memo.O in
                  let* compile_info =
                    let* scope = Scope.DB.find_by_dir dir in
                    let dune_version =
                      let project = Scope.project scope in
                      Dune_project.dune_version project
                    in
                    let+ pps =
                      Resolve.Memo.read_memo
                        (Preprocess.Per_module.with_instrumentation
                           exes.buildable.preprocess
                           ~instrumentation_backend:
                             (Lib.DB.instrumentation_backend (Scope.libs scope)))
                      >>| Preprocess.Per_module.pps
                    in
                    let names = Nonempty_list.to_list exes.names in
                    let merlin_ident =
                      Merlin_ident.for_exes ~names:(List.map ~f:snd names)
                    in
                    Lib.DB.resolve_user_written_deps
                      db
                      (`Exe names)
                      exes.buildable.libraries
                      ~pps
                      ~dune_version
                      ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
                      ~forbidden_libraries:exes.forbidden_libraries
                      ~merlin_ident
                  in
                  let+ available = Lib.Compile.direct_requires compile_info in
                  Resolve.peek available
                in
                (match libs with
                 | Error () -> acc
                 | Ok libs ->
                   List.fold_left libs ~init:acc ~f:(fun acc lib ->
                     Appendable_list.cons lib acc))
              | _ -> Memo.return acc))
    in
    Appendable_list.to_list libs
;;

type t = { dirs_to_include : String_with_vars.t list }

let gen_rule_for_source_file ~sctx ~dir ~ppx_driver_and_flags ~ml_kind path =
  let target = Path.Build.append_source dir (Path.drop_build_context_exn path) in
  (*let target = Path.Build.relative dir (Path.basename path) in*)
  let rule =
    Action_builder.with_file_targets
      ~file_targets:[ target ]
      (let open Action_builder.O in
       let* ppx_driver, flags = ppx_driver_and_flags in
       Command.run'
         ~dir:(Path.build dir)
         (Ok (Path.build ppx_driver))
         [ As flags
         ; Command.Ml_kind.ppx_driver_flag ml_kind
         ; Dep path
         ; A "-o"
         ; Path (Path.build target)
         ])
  in
  let mode = Rule.Mode.Promote { lifetime = Unlimited; into = None; only = None } in
  Super_context.add_rule sctx ~mode ~dir rule
;;

let gen_rules_for_module ~sctx ~dir ~ppx_driver_and_flags module_ =
  let impl = Module.file ~ml_kind:Impl module_ in
  let intf = Module.file ~ml_kind:Intf module_ in
  let* () =
    match impl with
    | None -> Memo.return ()
    | Some path ->
      gen_rule_for_source_file ~sctx ~dir ~ppx_driver_and_flags ~ml_kind:Impl path
  in
  match intf with
  | None -> Memo.return ()
  | Some path ->
    gen_rule_for_source_file ~sctx ~dir ~ppx_driver_and_flags ~ml_kind:Intf path
;;

let gen_rules_for_lib ~sctx ~dir ~scope ~expander lib =
  let* lib_to_preprocess = Lib_to_preprocess.from_lib ~sctx ~scope ~expander lib in
  match lib_to_preprocess with
  | None -> Memo.return ()
  | Some { name = _; sources; ppx_driver_and_flags } ->
    Memo.List.iter sources ~f:(gen_rules_for_module ~sctx ~dir ~ppx_driver_and_flags)
;;

let gen_stanza_rules ~dir ~dirs_to_include sctx =
  let* scope = Scope.DB.find_by_dir dir in
  let* expander = Super_context.expander sctx ~dir in
  let* libs =
    Memo.all (List.map dirs_to_include ~f:(libs_or_exes_under_dir ~sctx ~scope))
    >>| List.concat
  in
  Memo.List.iter libs ~f:(gen_rules_for_lib ~sctx ~dir ~scope ~expander)
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let syntax =
  let name = "include_preprocessed_sources" in
  let desc = "dummy" in
  Dune_lang.Syntax.create ~name ~desc [ (0, 1), `Since (3, 10) ]
;;

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ dirs_to_include = field "dirs" (repeat String_with_vars.decode) in
     { dirs_to_include })
;;

let () =
  let open Dune_lang.Decoder in
  let decode = Dune_lang.Syntax.since Stanza.syntax (3, 12) >>> decode in
  Dune_project.Extension.register_simple
    syntax
    (return
       [ ( "include_preprocessed_sources"
         , let+ stanza = decode in
           [ make_stanza stanza ] )
       ])
;;

module Gen_rules = Import.Build_config.Gen_rules

let rec under_include_preprocessed_sources ~dir =
  match Path.Build.parent dir with
  | None -> Memo.return false
  | Some parent ->
    Dune_load.stanzas_in_dir parent
    >>= (function
     | None -> under_include_preprocessed_sources ~dir:parent
     | Some dune_file ->
       let open Memo.O in
       let* stanzas = Dune_file.stanzas dune_file in
       let has_include_pp_src_stanza =
         List.exists stanzas ~f:(fun stanza ->
           match Stanza.repr stanza with
           | T _ -> true
           | _ -> false)
       in
       if has_include_pp_src_stanza
       then Memo.return true
       else under_include_preprocessed_sources ~dir:parent)
;;

let gen_sub_dir_rules ~dir =
  under_include_preprocessed_sources ~dir
  >>| function
  | true -> Gen_rules.redirect_to_parent Gen_rules.Rules.empty
  | false -> Gen_rules.no_rules
;;
