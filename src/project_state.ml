open Import

type t = {
  (* Projects that haven't been checked yet are absent. *)
  current_digests: Digest.t Dune_project.Name.Table.t;
  current_deps: Dune_project.Name.Set.t Dune_project.Name.Table.t;

  saved_digests: Digest.t Dune_project.Name.Table.t;
  saved_deps: Dune_project.Name.Set.t Dune_project.Name.Table.t;
}

type saved = {
  digests: Digest.t Dune_project.Name.Table.t;
  deps: Dune_project.Name.Set.t Dune_project.Name.Table.t;
}

let file = Path.relative Path.build_dir ".project-db"

module P = Utils.Persistent(struct
    type t = saved
    let name = "PROJECT-DB"
    let version = 1
  end)

let load () =
  let t = {
    current_digests = Dune_project.Name.Table.create 64;
    current_deps = Dune_project.Name.Table.create 64;
    saved_digests = Dune_project.Name.Table.create 64;
    saved_deps = Dune_project.Name.Table.create 64;
  }
  in
  match P.load file with
  | Some { digests; deps } ->
    { t with
      saved_digests = digests;
      saved_deps = deps;
    }
  | None -> t

(* Own digest is computed for a group of interdependent projects, namely
   a strongly connected component (SCC) in the project depedency graph.
   It only accounts for files in projects from the group, and doesn't
   account for dependencies on other projects.
*)
let compute_own_digest projects ~file_tree =
  List.fold_left projects ~init:[] ~f:(fun acc project ->
    let root = Dune_project.root project in
    match File_tree.find_dir file_tree (Path.of_local root) with
    | Some dir ->
      File_tree.Dir.fold
        dir
        ~traverse_ignored_dirs:false
        ~init:acc
        ~f:(fun dir acc ->
          Path.Set.fold
            (File_tree.Dir.file_paths dir)
            ~init:acc
            ~f:(fun p acc -> p :: acc))
    | None -> acc)
  |> List.sort ~compare:Path.compare
  |> List.map ~f:(fun path ->
    String.concat ~sep:":" [Path.to_string path; Utils.Cached_digest.file path])
  |> String.concat ~sep:";"
  |> Digest.string

(* This function computes current digests for root_project and all its
   direct or indirect dependencies. Digest for a project is computed
   from its own files and digests of all dependencies. Circular dependencies
   between projects are allowed.

   As soon as digest for a project is computed, saved digest is removed if
   it's different from the newly computed digest.
*)
let compute_digests t root_project ~projects ~file_tree =
  (* This function implements Tarjan's strongly connected components (SCC) algorithm. *)
  let module Local = struct
    type info = {
      mutable index: int;
      mutable low_link: int;
      mutable on_stack: bool;
    }
  end in
  let open Local in
  let module Table = Dune_project.Name.Table in
  let module Set = Dune_project.Name.Set in
  let next_index = ref 0 in
  let stack = Stack.create () in
  let info = Table.create 64 in
  let iter_deps project_name ~f =
    Option.iter (Table.find t.saved_deps project_name) ~f:(fun deps ->
      Dune_project.Name.Set.iter deps ~f)
  in
  (* This function assumes that the component dependencies are already computed. *)
  let compute_component_digest component =
    let own_digest =
      component
      |> Set.to_list
      |> List.map ~f:(Table.find projects)
      |> List.map ~f:Option.value_exn
      |> compute_own_digest ~file_tree
    in
    let all_deps = Set.fold component ~init:Set.empty ~f:(fun project acc ->
      match Table.find t.saved_deps project with
      | Some deps -> Set.union acc deps
      | None -> acc)
    in
    let external_deps = Set.diff all_deps component in
    let true_digest =
      Set.fold external_deps ~init:[own_digest] ~f:(fun dep acc ->
        Table.find_exn t.current_digests dep :: acc)
      |> String.concat ~sep:";"
      |> Digest.string
    in
    Set.iter component ~f:(fun project ->
      Table.replace t.current_digests ~key:project ~data:true_digest;
      match Table.find t.saved_digests project with
      | Some saved_digest ->
        if saved_digest <> true_digest then begin
          Table.remove t.saved_digests project;
          Table.remove t.saved_deps project
        end
      | None -> ())
  in
  let rec iter_strong_components v =
    let v_info =
      Dune_project.Name.Table.find_or_add
        info
        v
        ~f:(fun _ ->
          next_index := !next_index + 1;
          {
            index = !next_index;
            low_link = !next_index;
            on_stack = true
          })
    in
    Stack.push v stack;
    iter_deps v ~f:(fun dep ->
        match Table.find info dep with
        | Some dep_info ->
          if dep_info.on_stack then
            v_info.low_link <- min v_info.low_link dep_info.index
        | None ->
          iter_strong_components dep;
          v_info.low_link <- min v_info.low_link (Table.find_exn info dep).low_link
      );
    if v_info.low_link = v_info.index then begin
      let component = ref Set.empty in
      let should_continue = ref true in
      while !should_continue do
        let w = Stack.pop stack in
        (Table.find_exn info w).on_stack <- false;
        component := Set.add !component w;
        if w = v then
          should_continue := false
      done;
      (* Tarjan's algorithm outputs SCCs in reverse topological sorting order, so
         we can be sure that all dependencies are already processed *)
      compute_component_digest !component
    end
  in
  iter_strong_components root_project

let is_unclean t project ~projects ~file_tree =
  match Dune_project.Name.Table.(
    find t.current_digests project, find t.saved_digests project) with
  | Some current_digest, Some saved_digest ->
    (* If current digest was computed that's different from saved digest, saved digest
       should be immediately removed.
    *)
    assert (current_digest = saved_digest);
    false
  | Some _, None ->
    true
  | None, _ ->
    compute_digests t project ~projects ~file_tree;
    let current_digest =
      Dune_project.Name.Table.find_exn t.current_digests project
    in
    match Dune_project.Name.Table.find t.saved_digests project with
    | Some saved_digest ->
      current_digest <> saved_digest
    | None -> true

let register_dependency t ~target ~dependency =
  let deps =
    Dune_project.Name.Table.find t.current_deps target
    |> Option.value ~default:Dune_project.Name.Set.empty
  in
  let new_deps = Dune_project.Name.Set.add deps dependency in
  Dune_project.Name.Table.replace t.current_deps ~key:target ~data:new_deps

let mark_clean t project =
  let visited = ref Dune_project.Name.Set.empty in
  let rec go project =
    visited := Dune_project.Name.Set.add !visited project;
    match Dune_project.Name.Table.(
      find t.current_digests project,
      find_or_add t.current_deps project ~f:(fun _ -> Dune_project.Name.Set.empty)) with
    | Some current_digest, current_deps ->
      Dune_project.Name.Table.replace t.saved_digests ~key:project ~data:current_digest;
      Dune_project.Name.Table.replace t.saved_deps ~key:project ~data:current_deps;
      Dune_project.Name.Set.iter current_deps ~f:(fun dep ->
        if not (Dune_project.Name.Set.mem !visited dep) then
          go dep)
    | None, _ ->
      failwith "cannot mark clean project that wasn't checked for cleanliness before"
  in
  go project

let finalize t =
  if Path.build_dir_exists () then
    P.dump file { digests = t.saved_digests; deps = t.saved_deps }
