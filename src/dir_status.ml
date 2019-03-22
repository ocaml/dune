open Stdune
open Dune_file

module T = struct
  type is_component_of_a_group_but_not_the_root = {
    group_root : Path.t;
    stanzas : Stanza.t list Dir_with_dune.t option;
  }

  type t =
    | Standalone of
        (File_tree.Dir.t * Stanza.t list Dir_with_dune.t option) option
    (* Directory not part of a multi-directory group. The argument is
       [None] for directory that are not from the source tree, such as
       generated ones. *)

    | Group_root of File_tree.Dir.t
                    * Stanza.t list Dir_with_dune.t
    (* Directory with [(include_subdirs x)] where [x] is not [no] *)

    | Is_component_of_a_group_but_not_the_root of is_component_of_a_group_but_not_the_root
    (* Sub-directory of a [Group_root _] *)

  let to_sexp _ = Sexp.Atom "<dir-status is opaque>"
end
include T

type enclosing_group =
  | No_group
  | Group_root of Path.t

let current_group dir = function
  | Standalone _ -> No_group
  | Group_root _ -> Group_root dir
  | Is_component_of_a_group_but_not_the_root { group_root; _ } ->
    Group_root group_root

let get_include_subdirs stanzas =
  List.fold_left stanzas ~init:None ~f:(fun acc stanza ->
    match stanza with
    | Include_subdirs (loc, x) ->
      if Option.is_some acc then
        Errors.fail loc "The 'include_subdirs' stanza cannot appear \
                         more than once";
      Some x
    | _ -> acc)

let check_no_module_consumer stanzas =
  List.iter stanzas ~f:(fun stanza ->
    match stanza with
    | Library { buildable; _} | Executables { buildable; _ }
    | Tests { exes = { buildable; _ }; _ } ->
      Errors.fail buildable.loc
        "This stanza is not allowed in a sub-directory of directory with \
         (include_subdirs unqualified).\n\
         Hint: add (include_subdirs no) to this file."
    | _ -> ())

module DB = struct

  type nonrec t =
    { file_tree : File_tree.t
    ; stanzas_per_dir : Dune_file.Stanzas.t Dir_with_dune.t Path.Map.t
    ; fn : (Path.t, t) Memo.Sync.t
    }

  let stanzas_in db ~dir =
    Path.Map.find db.stanzas_per_dir dir

  let get db ~dir =
    let get ~dir = Memo.exec db.fn dir in
    let enclosing_group ~dir =
      match Path.parent dir with
      | None ->
        No_group
      | Some parent_dir ->
        current_group parent_dir (get ~dir:parent_dir)
    in
    match
      Option.bind (Path.drop_build_context dir)
        ~f:(File_tree.find_dir db.file_tree)
    with
    | None -> begin
        match enclosing_group ~dir  with
        | No_group -> Standalone None
        | Group_root group_root ->
          Is_component_of_a_group_but_not_the_root
            { stanzas = None; group_root }
      end
    | Some ft_dir ->
      let project_root =
        File_tree.Dir.project ft_dir
        |> Dune_project.root
        |> Path.of_local in
      match stanzas_in db ~dir with
      | None ->
        if Path.equal dir project_root  then
          Standalone (Some (ft_dir, None))
        else
          (match enclosing_group ~dir with
           | No_group ->
             Standalone (Some (ft_dir, None))
           | Group_root group_root ->
             Is_component_of_a_group_but_not_the_root
               { stanzas = None; group_root })
      | Some d ->
        match get_include_subdirs d.data with
        | Some Unqualified ->
          Group_root (ft_dir, d)
        | Some No ->
          Standalone (Some (ft_dir, Some d))
        | None ->
          if dir <> project_root
          then begin
            match enclosing_group ~dir with
            | Group_root group_root ->
              (
                check_no_module_consumer d.data;
                Is_component_of_a_group_but_not_the_root
                  { stanzas = (Some d); group_root })
            | No_group ->
              Standalone (Some (ft_dir, Some d))
          end else
            Standalone (Some (ft_dir, Some d))

  let make file_tree ~stanzas_per_dir =
    let t =
      { file_tree
      ; stanzas_per_dir
      ; fn =
          Memo.create
            "get-dir-status"
            ~input:(module Path)
            ~visibility:Hidden
            ~output:(Simple (module T))
            ~doc:"Get a directory status."
            Sync
            None
      }
    in
    Memo.set_impl t.fn (fun dir -> get t ~dir);
    t

  let get db ~dir =
    Memo.exec db.fn dir
end
