(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import
open Memo.O

type t = { scopes : Rocq_lib.DB.t Lazy.t Path.Source.Map.t Memo.Lazy.t }

let public_theories context public_libs rocq_stanzas =
  let+ installed_theories =
    let+ rocqpaths_of_rocq = Rocq_path.of_rocq_install context
    and+ rocqpaths_of_env = Context.installed_env context >>= Rocq_path.of_env in
    Rocq_lib.DB.create_from_rocqpaths (rocqpaths_of_env @ rocqpaths_of_rocq)
  in
  List.filter_map rocq_stanzas ~f:(fun (dir, (stanza : Rocq_stanza.Theory.t)) ->
    if Option.is_some stanza.package
    then Some (stanza, Rocq_lib.DB.Entry.Theory dir)
    else None)
  |> Rocq_lib.DB.create_from_rocqlib_stanzas
       ~find_db:(Fun.const public_libs)
       ~parent:(Some installed_theories)
;;

let rocq_scopes_by_dir
      db_by_project_dir
      projects_by_dir
      public_theories
      rocq_stanzas_by_project_dir
  =
  let parent = Some public_theories in
  let find_db dir =
    snd (Find_closest_source_dir.find_by_dir_exn db_by_project_dir ~dir)
  in
  Path.Source.Map.merge
    projects_by_dir
    rocq_stanzas_by_project_dir
    ~f:(fun _dir project rocq_stanzas ->
      assert (Option.is_some project);
      Option.some
      @@ lazy
           (let rocq_stanzas = Option.value rocq_stanzas ~default:[] in
            List.map rocq_stanzas ~f:(fun (dir, (stanza : Rocq_stanza.Theory.t)) ->
              let (entry : Rocq_lib.DB.Entry.t) =
                match stanza.package with
                | None -> Theory dir
                | Some _ -> Redirect public_theories
              in
              stanza, entry)
            |> Rocq_lib.DB.create_from_rocqlib_stanzas ~parent ~find_db))
;;

let rocq_stanzas_by_project_dir rocq_stanzas =
  List.map rocq_stanzas ~f:(fun (dir, (stanza : Rocq_stanza.Theory.t)) ->
    let project = stanza.project in
    Dune_project.root project, (dir, stanza))
  |> Path.Source.Map.of_list_multi
;;

let make context ~public_libs ~db_by_project_dir ~projects_by_root rocq_stanzas =
  { scopes =
      Memo.lazy_ (fun () ->
        let+ public_theories = public_theories context public_libs rocq_stanzas in
        let rocq_stanzas_by_project_dir = rocq_stanzas_by_project_dir rocq_stanzas in
        rocq_scopes_by_dir
          db_by_project_dir
          projects_by_root
          public_theories
          rocq_stanzas_by_project_dir)
  }
;;

let find t ~dir =
  let+ scopes = Memo.Lazy.force t.scopes in
  Path.Source.Map.find_exn scopes dir |> Lazy.force
;;
