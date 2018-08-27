open Import

module Partition = struct
  module T = struct
    type t =
      | Project of Dune_project.Name.t
      | Target of Path.t
      | Universe

    let compare a b =
      match a, b with
      | Universe , Universe  -> Ordering.Eq
      | Project x, Project y -> Dune_project.Name.compare x y
      | Target  x, Target  y -> Path.compare x y
      | Universe , _         -> Lt
      | _        , Universe  -> Gt
      | Project _, Target  _ -> Lt
      | Target  _, Project _ -> Gt

    let equal a b = compare a b = Eq

    let hash = Hashtbl.hash
  end

  include T

  let for_target target ~file_tree =
    if Path.equal target universe_file then
      Universe
    else
      match File_tree.project file_tree target with
      | Some project -> Project (Dune_project.name project)
      | None -> Target target

  let to_string_hum = function
    | Project project -> Dune_project.Name.to_string_hum project
    | Target target   -> Format.sprintf "<target: %s>" (Path.to_string target)
    | Universe        -> "(universe)"

  module Set = struct
    include Set.Make (T)
  end

  module Table = struct
    include Hashtbl.Make (T)
  end
end

type t = {
  (* Partitions that haven't been checked yet are absent. *)
  current_digests: Digest.t Partition.Table.t;
  current_deps: Partition.Set.t Partition.Table.t;

  saved_digests: Digest.t Partition.Table.t;
  saved_deps: Partition.Set.t Partition.Table.t;
}

type saved = {
  digests: Digest.t Partition.Table.t;
  deps: Partition.Set.t Partition.Table.t;
}

let file = Path.relative Path.build_dir ".partition-db"

module P = Utils.Persistent(struct
    type t = saved
    let name = "PARTITION-DB"
    let version = 1
  end)

let load () =
  let t = {
    current_digests = Partition.Table.create 64;
    current_deps = Partition.Table.create 64;
    saved_digests = Partition.Table.create 64;
    saved_deps = Partition.Table.create 64;
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
    Bytes.blit_string ~src:digest ~src_pos:0 ~dst:t.buffer ~dst_pos:0 ~len:16;
    t.size <- 16

  let update t content =
    if (String.length content > capacity - 16) then
      raise (Invalid_argument "Md5.update called with string that is too long")
    else if (String.length content) + t.size > capacity then
      compress t;
    Bytes.blit_string
      ~src:content
      ~src_pos:0
      ~dst:t.buffer
      ~dst_pos:t.size
      ~len:(String.length content);
    t.size <- t.size + (String.length content)

  let compute t =
    compress t;
    Bytes.sub_string t.buffer ~pos:0 ~len:16
end

(* Own digest is computed for a group of interdependent partitions, namely
   a strongly connected component (SCC) in the partition dependency graph.
   It only accounts for files in partitions from the group, and doesn't
   account for dependencies on other partitions.
*)
let compute_own_digest partitions ~projects ~file_tree =
  let digest = Md5.init () in
  Partition.Set.fold partitions ~init:[] ~f:(fun partition acc ->
    match partition with
    | Partition.Project project ->
      let root = Dune_project.root
                   (Dune_project.Name.Table.find_exn projects project)
      in
      (match File_tree.find_dir file_tree (Path.of_local root) with
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
      | None ->
        die "root of project %s is not an existing directory"
          (Dune_project.Name.to_string_hum project))
    | Partition.Target _ ->
      (* Targets have no own digests, and are fully expressed by their deps. *)
      acc
    | Partition.Universe ->
      (* (universe) should have a different digest every time. *)
      Md5.update digest (Format.sprintf "%f" (Unix.gettimeofday ()));
      acc)
  |> List.sort ~compare:Path.compare
  |> List.iter ~f:(fun path ->
    Md5.update digest (Path.to_string path);
    Md5.update digest (Utils.Cached_digest.file path));
  Md5.compute digest

(* This function computes current digests for root_partition and all its
   direct or indirect dependencies. Digest for a partition is computed
   from its own files and digests of all dependencies. Circular dependencies
   between partitions are allowed.

   As soon as digest for a partition is computed, saved digest is removed if
   it's different from the newly computed digest.
*)
let compute_digests t root_partition ~projects ~file_tree =
  (* This function implements Tarjan's strongly connected components (SCC) algorithm. *)
  let module Local = struct
    type info = {
      mutable index: int;
      mutable low_link: int;
      mutable on_stack: bool;
    }
  end in
  let open Local in
  let module Table = Partition.Table in
  let module Set = Partition.Set in
  let next_index = ref 0 in
  let stack = Stack.create () in
  let info = Table.create 64 in
  let iter_deps partition ~f =
    Option.iter (Table.find t.saved_deps partition) ~f:(fun deps -> Set.iter deps ~f)
  in
  (* This function assumes that the component dependencies digests are already computed. *)
  let compute_component_digest component =
    let own_digest = compute_own_digest component ~projects ~file_tree in
    let all_deps = Set.fold component ~init:Set.empty ~f:(fun partition acc ->
      match Table.find t.saved_deps partition with
      | Some deps -> Set.union acc deps
      | None -> acc)
    in
    let external_deps = Set.diff all_deps component in
    let true_digest_md5 = Md5.init () in
    Md5.update true_digest_md5 own_digest;
    Set.iter external_deps ~f:(fun dep ->
      Md5.update true_digest_md5 (Table.find_exn t.current_digests dep));
    let true_digest = Md5.compute true_digest_md5 in
    Set.iter component ~f:(fun partition ->
      Table.replace t.current_digests ~key:partition ~data:true_digest;
      (match Table.find t.saved_digests partition with
       | Some saved_digest ->
         if saved_digest <> true_digest || !Clflags.force then begin
           Table.remove t.saved_digests partition;
           Table.remove t.saved_deps partition;
           if !Clflags.debug_partition_cache then
             Format.eprintf
               "Partition dirty: %-25s (%s <- %s)"
               (Partition.to_string_hum partition)
               (Digest.to_hex true_digest)
               (Digest.to_hex saved_digest)
         end
         else begin
           Table.iter t.saved_deps ~f:(Table.replace t.current_deps);
           if !Clflags.debug_partition_cache then
             Format.eprintf
               "Partition clean: %-25s (%s)"
               (Partition.to_string_hum partition)
               (Digest.to_hex true_digest)
         end
       | None ->
         if !Clflags.debug_partition_cache then
           Format.eprintf
             "Partition dirty (no record): %-25s (%s)"
             (Partition.to_string_hum partition)
             (Digest.to_hex true_digest));
      if !Clflags.debug_partition_cache then begin
        let all_deps =
          Table.find_or_add t.saved_deps partition ~f:(fun _ -> Set.empty)
        in
        let important_deps = Partition.Set.filter all_deps ~f:(function
          | Partition.Project _ | Partition.Universe -> true
          | _ -> false)
        in
        let target_deps = Partition.Set.diff all_deps important_deps in
        let target_deps_count = Partition.Set.cardinal target_deps in
        Format.eprintf "\t [deps: %!";
        Set.to_list important_deps
        |> List.map ~f:Partition.to_string_hum
        |> String.concat ~sep:", "
        |> prerr_string;
        if target_deps_count > 0 then
          Format.eprintf ", ...%d target deps" target_deps_count;
        Format.eprintf "]\n%!"
      end)
  in
  let rec iter_strong_components v =
    let v_info =
      Table.find_or_add info v ~f:(fun _ ->
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
  iter_strong_components root_partition

let get_current_digest t partition ~projects ~file_tree =
  match Partition.Table.find t.current_digests partition with
  | Some digest -> digest
  | None ->
    compute_digests t partition ~projects ~file_tree;
    Partition.Table.find_exn t.current_digests partition

let is_unclean t partition ~projects ~file_tree =
  match Partition.Table.(
    find t.current_digests partition, find t.saved_digests partition) with
  | Some current_digest, Some saved_digest ->
    (* If current digest was computed that's different from saved digest, saved digest
       should be immediately removed.
    *)
    assert (current_digest = saved_digest);
    false
  | Some _, None ->
    true
  | None, _ ->
    let current_digest = get_current_digest t partition ~projects ~file_tree in
    match Partition.Table.find t.saved_digests partition with
    | Some saved_digest ->
      current_digest <> saved_digest
    | None -> true

let register_dependency t ~target ~dependency =
  if not (Partition.equal target dependency) then begin
    let deps =
      Partition.Table.find_or_add
        t.current_deps
        target
        ~f:(fun _ -> Partition.Set.empty)
    in
    let new_deps = Partition.Set.add deps dependency in
    Partition.Table.replace t.current_deps ~key:target ~data:new_deps
  end

let finalize t =
  if Path.build_dir_exists () then
    P.dump file { digests = t.current_digests; deps = t.current_deps }
