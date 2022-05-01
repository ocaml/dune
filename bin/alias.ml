open Stdune
open Dune_engine

type t =
  { name : Dune_engine.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune_rules.Context.t list
  }

let pp { name; recursive; dir; contexts = _ } =
  let open Pp.O in
  let s =
    (if recursive then "@" else "@@")
    ^ Path.Source.to_string
        (Path.Source.relative dir (Dune_engine.Alias.Name.to_string name))
  in
  let pp = Pp.verbatim "alias" ++ Pp.space ++ Pp.verbatim s in
  if recursive then Pp.verbatim "recursive" ++ Pp.space ++ pp else pp

let in_dir ~name ~recursive ~contexts dir =
  let checked = Util.check_path contexts dir in
  match checked with
  | External _ ->
    User_error.raise
      [ Pp.textf "@@ on the command line must be followed by a relative path" ]
  | In_source_dir dir -> { dir; recursive; name; contexts }
  | In_install_dir _ ->
    User_error.raise
      [ Pp.textf "Invalid alias: %s."
          (Path.to_string_maybe_quoted
             (Path.build Dune_engine.Dpath.Build.install_dir))
      ; Pp.textf "There are no aliases in %s." (Path.to_string_maybe_quoted dir)
      ]
  | In_build_dir (ctx, dir) ->
    { dir
    ; recursive
    ; name
    ; contexts =
        [ List.find_exn contexts ~f:(fun c ->
              Dune_engine.Context_name.equal
                (Dune_rules.Context.name c)
                ctx.name)
        ]
    }

let of_string (root : Workspace_root.t) ~recursive s ~contexts =
  let path = Path.relative Path.root (root.reach_from_root_prefix ^ s) in
  if Path.is_root path then
    User_error.raise
      [ Pp.textf "@ on the command line must be followed by a valid alias name"
      ]
  else
    let dir = Path.parent_exn path in
    let name = Dune_engine.Alias.Name.of_string (Path.basename path) in
    in_dir ~name ~recursive ~contexts dir

let find_dir_specified_on_command_line ~dir =
  let open Memo.O in
  Source_tree.find_dir dir >>| function
  | Some dir -> dir
  | None ->
    User_error.raise
      [ Pp.textf "Don't know about directory %s specified on the command line!"
          (Path.Source.to_string_maybe_quoted dir)
      ]

let dep_on_alias_multi_contexts ~dir ~name ~contexts =
  ignore (find_dir_specified_on_command_line ~dir : _ Memo.t);
  let context_to_alias_expansion ctx =
    let ctx_dir = Dune_engine.Context_name.build_dir ctx in
    let dir = Path.Build.(append_source ctx_dir dir) in
    Action_builder.alias (Dune_engine.Alias.make ~dir name)
  in
  Action_builder.all_unit (List.map contexts ~f:context_to_alias_expansion)

let dep_on_alias_rec_multi_contexts ~dir:src_dir ~name ~contexts =
  let open Action_builder.O in
  let* dir =
    Action_builder.of_memo (find_dir_specified_on_command_line ~dir:src_dir)
  in
  let+ is_nonempty_list =
    Action_builder.all
      (List.map contexts ~f:(fun ctx ->
           Action_builder.dep_on_alias_rec name ctx dir))
  in
  let is_nonempty = List.exists is_nonempty_list ~f:Fun.id in
  if (not is_nonempty) && not (Dune_engine.Alias.is_standard name) then
    User_error.raise
      [ Pp.textf "Alias %S specified on the command line is empty."
          (Dune_engine.Alias.Name.to_string name)
      ; Pp.textf "It is not defined in %s or any of its descendants."
          (Path.Source.to_string_maybe_quoted src_dir)
      ]

let request { name; recursive; dir; contexts } =
  let contexts = List.map ~f:Dune_rules.Context.name contexts in
  (if recursive then dep_on_alias_rec_multi_contexts
  else dep_on_alias_multi_contexts)
    ~dir ~name ~contexts
