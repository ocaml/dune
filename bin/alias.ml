open Stdune
open Import
open Dune_engine
module Source_tree = Dune_rules.Source_tree

type t =
  { name : Dune_engine.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; scontexts : Super_context.t Context_name.Map.t
  }

let pp { name; recursive; dir; scontexts = _ } =
  let open Pp.O in
  let s =
    (if recursive then "@" else "@@")
    ^ Path.Source.to_string
        (Path.Source.relative dir (Dune_engine.Alias.Name.to_string name))
  in
  let pp = Pp.verbatim "alias" ++ Pp.space ++ Pp.verbatim s in
  if recursive then Pp.verbatim "recursive" ++ Pp.space ++ pp else pp

let in_dir ~name ~recursive ~scontexts dir =
  let checked =
    let contexts =
      Context_name.Map.to_list_map scontexts ~f:(fun _ sctx ->
          Super_context.context sctx)
    in
    Util.check_path contexts dir
  in
  match checked with
  | External _ ->
    User_error.raise
      [ Pp.textf "@@ on the command line must be followed by a relative path" ]
  | In_source_dir dir -> { dir; recursive; name; scontexts }
  | In_install_dir _ ->
    User_error.raise
      [ Pp.textf "Invalid alias: %s."
          (Path.to_string_maybe_quoted (Path.build Dpath.Build.install_dir))
      ; Pp.textf "There are no aliases in %s." (Path.to_string_maybe_quoted dir)
      ]
  | In_build_dir (ctx, dir) ->
    { dir
    ; recursive
    ; name
    ; scontexts =
        Context_name.Map.(singleton ctx.name (find_exn scontexts ctx.name))
    }

let of_string (root : Workspace_root.t) ~recursive s ~scontexts =
  let path = Path.relative Path.root (root.reach_from_root_prefix ^ s) in
  if Path.is_root path then
    User_error.raise
      [ Pp.textf "@ on the command line must be followed by a valid alias name"
      ]
  else
    let dir = Path.parent_exn path in
    let name = Dune_engine.Alias.Name.of_string (Path.basename path) in
    in_dir ~name ~recursive ~scontexts dir

let check_dir_exists_in_source ~project ~src_dir =
  let open Memo.O in
  let dune_version = Dune_project.dune_version project in
  match Dune_lang.Syntax.Version.Infix.(dune_version >= (3, 8)) with
  | true -> Memo.return ()
  | false -> (
    Source_tree.find_dir src_dir >>| function
    | Some _ -> ()
    | None ->
      User_error.raise
        [ Pp.textf
            "Don't know about directory %s specified on the command line!"
            (Path.Source.to_string_maybe_quoted src_dir)
        ])

let project sctx ~dir =
  Action_builder.of_memo
    (let open Memo.O in
    let+ expander = Super_context.expander sctx ~dir in
    let scope = Dune_rules.Expander.scope expander in

    Dune_rules.Scope.project scope)

let dep_on_alias_multi_contexts ~dir:src_dir ~name ~scontexts =
  let context_to_alias_expansion ctx =
    let open Action_builder.O in
    let dir =
      let ctx_dir = Context_name.build_dir ctx in
      Path.Build.(append_source ctx_dir src_dir)
    in
    let* project =
      let sctx = Context_name.Map.find_exn scontexts ctx in
      project sctx ~dir
    in
    ignore (check_dir_exists_in_source ~project ~src_dir : unit Memo.t);
    Action_builder.alias (Dune_engine.Alias.make ~dir name)
  in
  Action_builder.all_unit
    (List.map (Context_name.Map.keys scontexts) ~f:context_to_alias_expansion)

let dep_on_alias_rec_multi_contexts ~dir:src_dir ~name ~scontexts =
  let open Action_builder.O in
  let+ alias_statuses =
    Action_builder.all
      (List.map (Context_name.Map.keys scontexts) ~f:(fun ctx ->
           let dir =
             Path.Build.append_source (Context_name.build_dir ctx) src_dir
           in
           let* project =
             let sctx = Context_name.Map.find_exn scontexts ctx in
             project sctx ~dir
           in
           ignore (check_dir_exists_in_source ~project ~src_dir : unit Memo.t);
           Dune_rules.Alias_rec.dep_on_alias_rec ~project name dir))
  in
  let is_nonempty =
    List.exists alias_statuses
      ~f:(fun (x : Dune_rules.Alias_rec.Alias_status.t) ->
        match x with
        | Defined -> true
        | Not_defined -> false)
  in
  if (not is_nonempty) && not (Dune_engine.Alias.is_standard name) then
    User_error.raise
      [ Pp.textf "Alias %S specified on the command line is empty."
          (Dune_engine.Alias.Name.to_string name)
      ; Pp.textf "It is not defined in %s or any of its descendants."
          (Path.Source.to_string_maybe_quoted src_dir)
      ]

let request { name; recursive; dir; scontexts } =
  (if recursive then dep_on_alias_rec_multi_contexts
  else dep_on_alias_multi_contexts)
    ~dir ~name ~scontexts
