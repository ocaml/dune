(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2019                    *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

type t = { db : Coq_lib.DB.t }

let libs { db; _ } = db

module DB = struct
  type scope = t

  type t =
    { by_dir : scope Path.Source.Map.t
    ; context : Context_name.t
    }

  let find_by_dir t (dir : Path.Source.t) =
    let rec loop d =
      match Path.Source.Map.find t.by_dir d with
      | Some s -> s
      | None -> (
        match Path.Source.parent d with
        | Some d -> loop d
        | None ->
          Code_error.raise "find_by_dir: invalid directory"
            [ ("d", Path.Source.to_dyn d); ("dir", Path.Source.to_dyn dir) ] )
    in
    loop dir

  let find_by_project t project =
    Path.Source.Map.find_exn t.by_dir (Dune_project.root project)

  let scopes_by_dir stanzas =
    let stanzas_by_project_dir =
      List.map stanzas
        ~f:(fun ((dir, stanza) : Path.Build.t * Dune_file.Coq.t) ->
          let project = stanza.project in
          (Dune_project.root project, (dir, stanza)))
      |> Path.Source.Map.of_list_multi
    in
    Path.Source.Map.map stanzas_by_project_dir ~f:(fun stanza ->
        let db = Coq_lib.DB.create_from_coqlib_stanzas stanza in
        { db })

  let create ~context stanzas =
    let by_dir = scopes_by_dir stanzas in
    { by_dir; context }

  let find_by_dir t dir =
    if Path.Build.is_root dir then
      Code_error.raise "Scope.DB.find_by_dir got an invalid path"
        [ ("dir", Path.Build.to_dyn dir)
        ; ("context", Context_name.to_dyn t.context)
        ];
    find_by_dir t (Path.Build.drop_build_context_exn dir)
end
