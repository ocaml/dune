open Import
module CC = Compilation_context
module SC = Super_context

let activated =
  let open Memo.O in
  let+ workspace = Workspace.workspace () in
  workspace.config.workspace_indexation = Enabled

let ocaml_uideps sctx ~dir =
  Super_context.resolve_program ~loc:None ~dir sctx "ocaml-uideps"

let uideps_path_in_obj_dir ?for_cmt obj_dir =
  let dir = Obj_dir.obj_dir obj_dir in
  match for_cmt with
  | Some path ->
    let name = Path.basename path in
    Path.Build.relative dir @@ Printf.sprintf ".uideps/%s.uideps" name
  | None -> Path.Build.relative dir "cctx.uideps"

let build_path cctx =
  let open Memo.O in
  let+ requires_link = Compilation_context.requires_link cctx in
  let requires =
    match Resolve.peek requires_link with
    | Ok l -> Lib.Set.of_list l
    | Error () -> Lib.Set.empty
  in
  Lib.Set.to_list_map requires ~f:(fun lib ->
      let info = Lib.info lib in
      Lib_info.obj_dir info |> Obj_dir.byte_dir)

let cctx_rules cctx =
  let open Memo.O in
  let* activated = activated in
  if not activated then Memo.return ()
  else
    (* Indexing is performed by the external binary [ocaml-uideps] which performs
        full shape reduction to compute the actual definition of all the elements in
        the typedtree. This step is therefore dependent on all the cmts of those
        definitions are used by all the cmts of modules in this cctx. *)
    let dir = CC.dir cctx in
    let modules =
      CC.modules cctx
      |> Modules.fold_no_vlib ~init:[] ~f:(fun x acc -> x :: acc)
    in
    let sctx = CC.super_context cctx in
    let obj_dir = CC.obj_dir cctx in
    let cm_kind = Lib_mode.Cm_kind.(Ocaml Cmi) in
    let modules_with_cmts =
      List.filter_map
        ~f:(fun module_ ->
          Obj_dir.Module.cmt_file obj_dir ~ml_kind:Impl ~cm_kind module_
          |> Option.map ~f:(fun cmt -> (module_, Path.build cmt)))
        modules
    in
    let* ocaml_uideps = ocaml_uideps sctx ~dir in
    let context_dir =
      CC.context cctx |> Context.name |> Context_name.build_dir |> Path.build
    in
    let* build_path = build_path cctx in
    (* let all_cmts = List.map ~f:snd modules_with_cmts in *)
    let intermediate_targets, intermediates =
      List.fold_map modules_with_cmts ~init:[]
        ~f:(fun targets (_module_, for_cmt) ->
          let fn = uideps_path_in_obj_dir ~for_cmt obj_dir in
          let action =
            Command.run ~dir:context_dir ocaml_uideps
              [ A "process-cmt"
              ; A "--root"
              ; A Path.(Source.root |> source |> to_absolute_filename)
              ; A "-o"
              ; Target fn
              ; Dep for_cmt
              ; As (List.map ~f:Path.to_absolute_filename build_path)
                (* TODO ulysse: these deps are incorrect. Fixing it might improve
                   performance by increasing parallelism. *)
                (* ; Hidden_deps (Dep.Set.of_files all_cmts) *)
              ]
          in
          (Path.build fn :: targets, action))
    in
    let fn = uideps_path_in_obj_dir obj_dir in
    let aggregate =
      Command.run ~dir:context_dir ocaml_uideps
        [ A "aggregate"; A "-o"; Target fn; Deps intermediate_targets ]
    in
    SC.add_rules sctx ~dir (aggregate :: intermediates)

let aggregate sctx ~dir ~target ~uideps =
  let open Memo.O in
  if List.is_empty uideps then Memo.return ()
  else
    let* ocaml_uideps = ocaml_uideps sctx ~dir in
    let uideps = List.map ~f:Path.build uideps in
    SC.add_rule sctx ~dir
      (Command.run ~dir:(Path.build dir) ocaml_uideps
         [ A "aggregate"; A "-o"; Target target; Deps uideps ])

let project_rule sctx project =
  let open Memo.O in
  let* activated = activated in
  if not activated then Memo.return ()
  else
    let ctx = Super_context.context sctx in
    let dir =
      Path.Build.append_source ctx.build_dir @@ Dune_project.root project
    in
    let* stanzas = Only_packages.filtered_stanzas ctx in
    let* expander =
      let+ expander = Super_context.expander sctx ~dir in
      Dir_contents.add_sources_to_expander sctx expander
    in
    let scope = Expander.scope expander in
    let* uideps =
      (* We only index public stanzas of vendored libs *)
      Dune_file.fold_stanzas stanzas ~init:(Memo.return [])
        ~f:(fun dune_file stanza acc ->
          let dir = Path.Build.append_source ctx.build_dir dune_file.dir in
          let* vendored = Super_context.build_dir_is_vendored dir in
          let* obj =
            let open Dune_file in
            match stanza with
            | Executables exes ->
              if vendored then Memo.return None
              else
                Memo.return
                @@ Some (Executables.obj_dir ~dir exes, exes.enabled_if)
            | Library lib ->
              let public =
                match lib.visibility with
                | Public _ -> true
                | Private _ -> false
              in
              if vendored && not public then Memo.return None
              else
                let+ available =
                  if lib.optional then
                    Lib.DB.available (Scope.libs scope)
                      (Dune_file.Library.best_name lib)
                  else Memo.return true
                in
                if available then Some (Library.obj_dir ~dir lib, lib.enabled_if)
                else None
            | _ -> Memo.return None
          in
          match obj with
          | None -> acc
          | Some (obj_dir, enabled_if) ->
            let* enabled = Expander.eval_blang expander enabled_if in
            if enabled then
              let+ acc = acc in
              uideps_path_in_obj_dir obj_dir :: acc
            else acc)
    in
    let target = Path.Build.relative dir "project.uideps" in
    let uideps_alias = Alias.uideps ~dir in
    let* () =
      Rules.Produce.Alias.add_deps uideps_alias
        (Action_builder.path @@ Path.build target)
    in
    aggregate sctx ~dir ~target ~uideps
