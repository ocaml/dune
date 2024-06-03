open Import
open Memo.O

module Lib_or_exes_to_pp = struct
  type t =
    { ppx_driver_and_flags : (Path.Build.t * string list) Action_builder.t
    ; sources : Module.t list
    ; source_dir : Path.Build.t
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

  let modules_to_pp ms =
    Modules.fold_user_written ms ~init:[] ~f:(fun module_ acc -> module_ :: acc)
  ;;

  let sources ~sctx lib =
    let+ modules = Dir_contents.modules_of_lib sctx lib in
    match modules with
    | None -> []
    | Some ms -> modules_to_pp ms
  ;;

  let from_lib ~sctx ~scope ~expander ~dir lib =
    let* ppx_driver_and_flags = ppx_driver ~sctx ~scope ~expander lib in
    match ppx_driver_and_flags with
    | Some ppx_driver_and_flags ->
      let+ sources = sources ~sctx lib in
      Some { ppx_driver_and_flags; sources; source_dir = dir }
    | None -> Memo.return None
  ;;

  let from_exes ~sctx ~scope ~expander ~dir (exes : Executables.t) =
    let libs = Scope.libs scope in
    let* triage = Dir_contents.triage sctx ~dir in
    let* dir_contents =
      match triage with
      | Standalone_or_root sor -> Dir_contents.Standalone_or_root.root sor
      | Group_part _ -> assert false
    in
    let names = Nonempty_list.to_list exes.names in
    let first_exe = snd (List.hd names) in
    let* sources =
      let* ml_sources = Dir_contents.ocaml dir_contents in
      let+ modules = Ml_sources.modules ~libs ~for_:(Exe { first_exe }) ml_sources in
      modules_to_pp modules
    in
    let pp = exes.buildable.preprocess in
    let pps =
      Preprocess.Per_module.pps (Preprocess.Per_module.without_instrumentation pp)
    in
    match pps with
    | [] -> Memo.return None
    | _ ->
      let ctx = Super_context.context sctx in
      let ppx_driver_and_flags =
        Ppx_driver.get_ppx_driver
          ~loc:Loc.none
          ~expander
          ~scope
          ~lib_name:None
          ~flags:[]
          ctx
          pps
      in
      Memo.return (Some { sources; ppx_driver_and_flags; source_dir = dir })
  ;;
end

module To_pp_list = Monoid.Appendable_list (struct
    type t = Lib_or_exes_to_pp.t
  end)

module Source_tree_map_reduce = Source_tree.Dir.Make_map_reduce (Memo) (To_pp_list)

let is_descendant_of ~dirs_to_exclude dir =
  let dir = Source_tree.Dir.path dir in
  List.exists dirs_to_exclude ~f:(fun to_exclude ->
    Path.Source.is_descendant dir ~of_:to_exclude)
;;

let libs_or_exes_to_pp_in_source_tree ~sctx ~scope ~expander ~dirs_to_exclude =
  let append_opt opt acc =
    match opt with
    | None -> acc
    | Some v -> Appendable_list.cons v acc
  in
  let db = Scope.libs scope in
  let* dir = Source_tree.root () in
  let+ srcs_to_pp =
    Source_tree_map_reduce.map_reduce
      dir
      ~traverse:Source_dir_status.Set.all
      ~f:(fun dir ->
        if is_descendant_of ~dirs_to_exclude dir
        then Memo.return To_pp_list.empty
        else (
          let build_dir = Context.build_dir (Super_context.context sctx) in
          let dir = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
          Dune_load.stanzas_in_dir dir
          >>= function
          | None -> Memo.return To_pp_list.empty
          | Some (d : Dune_file.t) ->
            let* stanzas = Dune_file.stanzas d in
            Memo.List.fold_left stanzas ~init:To_pp_list.empty ~f:(fun acc stanza ->
              match Stanza.repr stanza with
              | Library.T l ->
                let* lib =
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
                   Memo.return acc
                 | Some (Ok lib) ->
                   (* still need to make sure that it's not coming from an
                      external source *)
                   let info = Lib.info lib in
                   let src_dir = Lib_info.src_dir info in
                   if Path.is_descendant ~of_:(Path.build dir) src_dir
                   then
                     let+ src_to_pp =
                       Lib_or_exes_to_pp.from_lib ~sctx ~scope ~expander ~dir lib
                     in
                     append_opt src_to_pp acc
                   else Memo.return acc)
              | Executables.T exes ->
                let+ src_to_pp =
                  Lib_or_exes_to_pp.from_exes ~sctx ~scope ~expander ~dir exes
                in
                append_opt src_to_pp acc
              | _ -> Memo.return acc)))
  in
  Appendable_list.to_list srcs_to_pp
;;

type pp_ctx =
  { sctx : Super_context.t
  ; expander : Expander.t
  ; dialects : Dialect.DB.t
  ; stanza_dir : Path.Build.t
  ; source_dir : Path.Build.t
  ; format_config : Format_config.t
  ; ppx_driver_and_flags : (Path.Build.t * string list) Action_builder.t
  ; alias : Alias.t
  }

let gen_pp_action ~pp_ctx ~ml_kind ~target ~input =
  let open Action_builder.O in
  let* ppx_driver, flags = pp_ctx.ppx_driver_and_flags in
  Command.run'
    ~dir:(Path.build pp_ctx.stanza_dir)
    (Ok (Path.build ppx_driver))
    [ As flags
    ; Command.Ml_kind.ppx_driver_flag ml_kind
    ; Dep input
    ; A "-dune-optional-output"
    ; A "-o"
    ; Path (Path.build target)
    ]
;;

let gen_format_action ~pp_ctx ~output ~ext ~input =
  Format_rules.format_action
    ~expander:pp_ctx.expander
    ~dialects:pp_ctx.dialects
    ~config:pp_ctx.format_config
    ~ext
    ~input
    ~dir:pp_ctx.source_dir
    ~output
;;

let gen_rules_for_source_file ~pp_ctx ~ml_kind path =
  let target =
    Path.Build.append_source pp_ctx.stanza_dir (Path.drop_build_context_exn path)
  in
  let raw_pp_target = Path.Build.map_extension target ~f:(fun ext -> ".pp" ^ ext) in
  let ext = Path.extension path in
  let combined_action =
    let open Action_builder.O in
    let* pp_action = gen_pp_action ~pp_ctx ~ml_kind ~target:raw_pp_target ~input:path in
    let+ format_action =
      match gen_format_action ~pp_ctx ~output:target ~ext ~input:raw_pp_target with
      | Some format ->
        (* XXX this needs to to be a no-op in case the pp_action didn't produce
           anything.

           One way to do it is to create a custom action:

           {[
             module Spec = struct
               type ('path, 'target) t = 'path * Action.t
               ...
             end

             val run_if_exists : Path.t -> Action.t -> Action.t
           ]}

           See [Copy_line_directive] for how to create a custom action. You will
           need to call out to [Action_exec] to execute the underlying action.

           The simpler way to do it just to hardcode the action directly into
           [dune_engine/action.ml].
        *)
        Action_builder.map format.build ~f:(fun full_act ->
          Action.Full.map full_act ~f:(fun action ->
            Action.run_if_exists (Path.build raw_pp_target) action))
      | None -> Action_builder.return Action.Full.empty
    in
    Action.Full.combine
      (Action.Full.combine pp_action format_action)
      (Action.Full.make (Action.diff ~optional:true path target))
  in
  Super_context.add_alias_action
    pp_ctx.sctx
    pp_ctx.alias
    ~dir:pp_ctx.stanza_dir
    ~loc:Loc.none
    combined_action
;;

let gen_rules_for_module ~pp_ctx module_ =
  let impl = Module.file ~ml_kind:Impl module_ in
  let intf = Module.file ~ml_kind:Intf module_ in
  let* () =
    match impl with
    | None -> Memo.return ()
    | Some path -> gen_rules_for_source_file ~pp_ctx ~ml_kind:Impl path
  in
  match intf with
  | None -> Memo.return ()
  | Some path -> gen_rules_for_source_file ~pp_ctx ~ml_kind:Intf path
;;

let gen_rules_for_lib_or_exes ~sctx ~expander ~dir ~dialects ~alias lib_or_exes_to_pp =
  let { Lib_or_exes_to_pp.sources; ppx_driver_and_flags; source_dir } =
    lib_or_exes_to_pp
  in
  Format_rules.with_config ~dir:source_dir (fun format_config ->
    let pp_ctx =
      { sctx
      ; expander
      ; dialects
      ; stanza_dir = dir
      ; ppx_driver_and_flags
      ; source_dir
      ; format_config
      ; alias
      }
    in
    Memo.List.iter sources ~f:(gen_rules_for_module ~pp_ctx))
;;

let gen_stanza_rules ~dir ~dirs_to_exclude sctx =
  let* scope = Scope.DB.find_by_dir dir in
  let* expander = Super_context.expander sctx ~dir in
  let alias = Alias.make Alias0.check ~dir in
  let project = Scope.project scope in
  let dialects = Dune_project.dialects project in
  let dirs_to_exclude = List.map dirs_to_exclude ~f:Path.drop_build_context_exn in
  let* libs_or_exes_to_pp =
    libs_or_exes_to_pp_in_source_tree ~sctx ~scope ~expander ~dirs_to_exclude
  in
  Memo.List.iter
    libs_or_exes_to_pp
    ~f:(gen_rules_for_lib_or_exes ~expander ~dialects ~sctx ~dir ~alias)
;;

type t = { dirs_to_exclude : String_with_vars.t list }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let syntax =
  let name = "include_preprocessed_sources" in
  let desc = "private extension to allow use of ppx in dune" in
  Dune_lang.Syntax.create ~name ~desc [ (0, 1), `Since (3, 10) ]
;;

let project_is_dune project =
  let project_name = Dune_project.name project in
  match Dune_project_name.name project_name with
  | Some "dune" -> true
  | _ -> false
;;

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ project = Dune_project.get_exn ()
     and+ dirs_to_exclude =
       field ~default:[] "exclude_dirs" (repeat String_with_vars.decode)
     in
     if project_is_dune project
     then { dirs_to_exclude }
     else User_error.raise ~loc [ Pp.text "This stanza is reserved for Dune itself" ])
;;

let () =
  let open Dune_lang.Decoder in
  let decode = Dune_lang.Syntax.since Stanza.syntax (3, 13) >>> decode in
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
