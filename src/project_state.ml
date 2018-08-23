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

module Md5 : sig
  type t
  val init : unit -> t
  val update : t -> string -> unit
  val compute : t -> Digest.t
end = struct
  type t = {
    buffer: bytes;
    mutable size: int;
  }

  let capacity = 65536

  let init () =
    { buffer = Bytes.create capacity
    ; size = 0
    }

  let compress t =
    let digest = Digest.subbytes t.buffer 0 t.size in
    Bytes.blit_string digest 0 t.buffer 0 16;
    t.size <- 16

  let update t content =
    if (String.length content > capacity - 16) then
      raise (Invalid_argument "Md5.update called with string that is too long")
    else if (String.length content) + t.size > capacity then
      compress t;
    Bytes.blit_string content 0 t.buffer t.size (String.length content);
    t.size <- t.size + (String.length content)

  let compute t =
    compress t;
    Bytes.sub_string t.buffer 0 16
end

(* Own digest is computed for a group of interdependent projects, namely
   a strongly connected component (SCC) in the project depedency graph.
   It only accounts for files in projects from the group, and doesn't
   account for dependencies on other projects.
*)
let compute_own_digest projects ~file_tree =
  let digest = Md5.init () in
  List.fold_left projects ~init:[] ~f:(fun acc project ->
    let root = Dune_project.root project in
    match File_tree.find_dir file_tree (Path.of_local root) with
    | Some dir ->
      File_tree.Dir.fold
        dir
        ~stay_in_project:true
        ~traverse_ignored_dirs:false
        ~init:acc
        ~f:(fun dir acc ->
          Path.Set.fold
            (File_tree.Dir.file_paths dir)
            ~init:acc
            ~f:(fun p acc -> p :: acc))
    | None -> acc)
  |> List.sort ~compare:Path.compare
  |> List.iter ~f:(fun path ->
    Md5.update digest (Path.to_string path);
    Md5.update digest (Utils.Cached_digest.file path));
  Md5.compute digest

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
    let true_digest_md5 = Md5.init () in
    Md5.update true_digest_md5 own_digest;
    Set.iter external_deps ~f:(fun dep ->
      Md5.update true_digest_md5 (Table.find_exn t.current_digests dep));
    let true_digest = Md5.compute true_digest_md5 in
    Set.iter component ~f:(fun project ->
      Table.replace t.current_digests ~key:project ~data:true_digest;
      (match Table.find t.saved_digests project with
       | Some saved_digest ->
         if saved_digest <> true_digest then begin
           Table.remove t.saved_digests project;
           Table.remove t.saved_deps project;
           if !Clflags.debug_partition_cache then
             Format.eprintf
               "Project dirty: %-25s (%s <- %s)"
               (Dune_project.Name.to_string_hum project)
               (Digest.to_hex true_digest)
               (Digest.to_hex saved_digest)
         end
         else begin
           Table.iter t.saved_deps ~f:(Table.replace t.current_deps);
           if !Clflags.debug_partition_cache then
             Format.eprintf
               "Project clean: %-25s (%s)"
               (Dune_project.Name.to_string_hum project)
               (Digest.to_hex true_digest)
         end
       | None ->
         if !Clflags.debug_partition_cache then
           Format.eprintf
             "Project dirty (no record): %-25s (%s)"
             (Dune_project.Name.to_string_hum project)
             (Digest.to_hex true_digest));
      if !Clflags.debug_partition_cache then begin
        Format.eprintf "\t [sdeps: %!";
        Table.find_or_add t.saved_deps project ~f:(fun _ -> Set.empty)
        |> Set.to_list
        |> List.map ~f:Dune_project.Name.to_string_hum
        |> String.concat ~sep:", "
        |> prerr_string;
        Format.eprintf "]\n%!"
      end)
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
          (* If dependency digest is already computed, then it's for sure
             in a different SCC that was already processed before, don't
             process it again.
          *)
          if not (Table.mem t.current_digests dep) then begin
            iter_strong_components dep;
            v_info.low_link <- min v_info.low_link (Table.find_exn info dep).low_link
          end);
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

let get_current_digest t project ~projects ~file_tree =
  match Dune_project.Name.Table.find t.current_digests project with
  | Some digest -> digest
  | None ->
    compute_digests t project ~projects ~file_tree;
    Dune_project.Name.Table.find_exn t.current_digests project

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
    let current_digest = get_current_digest t project ~projects ~file_tree in
    match Dune_project.Name.Table.find t.saved_digests project with
    | Some saved_digest ->
      current_digest <> saved_digest
    | None -> true

let register_dependency t ~target ~dependency =
  if Dune_project.Name.compare target dependency <> Ordering.Eq then begin
    let deps =
      Dune_project.Name.Table.find_or_add
        t.current_deps
        target
        ~f:(fun _ -> Dune_project.Name.Set.empty)
    in
    let new_deps = Dune_project.Name.Set.add deps dependency in
    Dune_project.Name.Table.replace t.current_deps ~key:target ~data:new_deps
  end

let finalize t =
  if Path.build_dir_exists () then
    P.dump file { digests = t.current_digests; deps = t.current_deps }
