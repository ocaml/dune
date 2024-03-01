open Import
open Memo.O

type t = { scopes : Coq_lib.DB.t Lazy.t Path.Source.Map.t Memo.Lazy.t }

let public_theories context public_libs coq_stanzas =
  let+ installed_theories =
    let+ coqpaths_of_coq = Coq_path.of_coq_install context
    and+ coqpaths_of_env = Context.installed_env context >>= Coq_path.of_env in
    Coq_lib.DB.create_from_coqpaths (coqpaths_of_env @ coqpaths_of_coq)
  in
  List.filter_map coq_stanzas ~f:(fun (dir, (stanza : Coq_stanza.Theory.t)) ->
    if Option.is_some stanza.package
    then Some (stanza, Coq_lib.DB.Entry.Theory dir)
    else None)
  |> Coq_lib.DB.create_from_coqlib_stanzas
       ~find_db:(Fun.const public_libs)
       ~parent:(Some installed_theories)
;;

let coq_scopes_by_dir
  db_by_project_dir
  projects_by_dir
  public_theories
  coq_stanzas_by_project_dir
  =
  let parent = Some public_theories in
  let find_db dir = snd (Find_closest_source_dir.find_by_dir db_by_project_dir ~dir) in
  Path.Source.Map.merge
    projects_by_dir
    coq_stanzas_by_project_dir
    ~f:(fun _dir project coq_stanzas ->
      assert (Option.is_some project);
      Option.some
      @@ lazy
           (let coq_stanzas = Option.value coq_stanzas ~default:[] in
            List.map coq_stanzas ~f:(fun (dir, (stanza : Coq_stanza.Theory.t)) ->
              let (entry : Coq_lib.DB.Entry.t) =
                match stanza.package with
                | None -> Theory dir
                | Some _ -> Redirect public_theories
              in
              stanza, entry)
            |> Coq_lib.DB.create_from_coqlib_stanzas ~parent ~find_db))
;;

let coq_stanzas_by_project_dir coq_stanzas =
  List.map coq_stanzas ~f:(fun (dir, (stanza : Coq_stanza.Theory.t)) ->
    let project = stanza.project in
    Dune_project.root project, (dir, stanza))
  |> Path.Source.Map.of_list_multi
;;

let make context ~public_libs ~db_by_project_dir ~projects_by_root coq_stanzas =
  { scopes =
      Memo.lazy_ (fun () ->
        let+ public_theories = public_theories context public_libs coq_stanzas in
        let coq_stanzas_by_project_dir = coq_stanzas_by_project_dir coq_stanzas in
        coq_scopes_by_dir
          db_by_project_dir
          projects_by_root
          public_theories
          coq_stanzas_by_project_dir)
  }
;;

let find t ~dir =
  let+ scopes = Memo.Lazy.force t.scopes in
  Path.Source.Map.find_exn scopes dir |> Lazy.force
;;
