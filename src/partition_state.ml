open Import

module Partition = struct
  module T = struct
    module Env_var = struct
      type t =
        { var: string
        ; context: string
        }

      let compare a b =
        match String.compare a.var b.var with
        | Eq ->
          String.compare a.context b.context
        | ord -> ord
    end

    type t =
      | Project of Dune_project.Name.t
      | Dir of Path.t
      | Target of Path.t
      | Env_var of Env_var.t
      | Universe

    let compare a b =
      match a, b with
      | Project x, Project y -> Dune_project.Name.compare x y
      | Dir     x, Dir     y -> Path.compare x y
      | Target  x, Target  y -> Path.compare x y
      | Env_var x, Env_var y -> Env_var.compare x y
      | Universe , Universe  -> Ordering.Eq
      | Project _, _         -> Lt
      | _        , Project _ -> Gt
      | Dir     _, _         -> Lt
      | _        , Dir     _ -> Gt
      | Target  _, _         -> Lt
      | _        , Target  _ -> Gt
      | Env_var _, _         -> Lt
      | _        , Env_var _ -> Gt

    let equal a b = compare a b = Eq

    let hash = Hashtbl.hash
  end

  include T

  let for_path path ~file_tree =
    if Path.equal path universe_file then
      Universe
    else
      match File_tree.project file_tree path with
      | Some project ->
        if Dune_project.(Name.equal (name project) Name.anonymous_root) then
          (* Instead of assigning paths to <anonymous .>, use dir partitions. *)
          let dir =
            Path.drop_optional_alias_dir path
            |> Path.drop_optional_build_context
            |> Path.parent_exn
          in
          Dir dir
        else
          Project (Dune_project.name project)
      | None ->
        (* We assume here that all source files can be successfully mapped to projects. *)
        Target path

  let for_env_var ~var ~context =
    Env_var { var; context }

  let to_string_hum = function
    | Project project          -> Dune_project.Name.to_string_hum project
    | Dir dir                  -> Format.sprintf "<dir: %s>" (Path.to_string dir)
    | Target target            -> Format.sprintf "<target: %s>" (Path.to_string target)
    | Env_var { var; context } -> Format.sprintf "(env: %s in %s)" var context
    | Universe                 -> "(universe)"

  module Set = struct
    include Set.Make (T)
  end

  module Table = struct
    include Hashtbl.Make (T)
  end
end

type t = {
  (* current_* tables contain information about partitions
     that have already been checked by [is_clean] or [get_current_digest]. *)
  current_digests: Digest.t Partition.Table.t;
  current_deps: Partition.Set.t Partition.Table.t;

  (* saved_* tables contain information from previous run loaded from disk. *)
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

(* A helper module for computing large digests incrementally. *)
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
let compute_own_digest partitions ~projects ~file_tree ~contexts =
  let digest = Md5.init () in
  let paths = ref [] in
  let env_vars = ref [] in
  let walk_dir_tree root =
    File_tree.Dir.fold
      root
      ~stay_in_project:true
      ~traverse_ignored_dirs:false
      ~init:()
      ~f:(fun dir _ ->
        Path.Set.iter
          (File_tree.Dir.file_paths dir)
          ~f:(fun p -> paths := p :: !paths))
  in
  Partition.Set.iter partitions ~f:(fun partition ->
    match partition with
    | Partition.Project project ->
      (match Dune_project.Name.Table.find projects project with
       | Some proj ->
         let root = Dune_project.root proj in
         (match File_tree.find_dir file_tree (Path.of_local root) with
          | Some root ->
            walk_dir_tree root
          | None ->
            die "root of project %s is not an existing directory"
              (Dune_project.Name.to_string_hum project))
       | None ->
         (* The project might have been deleted or renamed since the last run. *)
         ())
    | Partition.Dir dir ->
      (match File_tree.find_dir file_tree dir with
       | Some dir ->
         walk_dir_tree dir
       | None ->
         (* Dir no longer exists. *)
         ())
    | Partition.Target _ ->
      (* Targets have no own digests, and are fully expressed by their deps. *)
      ()
    | Partition.Env_var var ->
      env_vars := var :: !env_vars
    | Partition.Universe ->
      (* (universe) should have a different digest every time.
         It should be only encountered once, so it's safe to update digest immediately. *)
      Md5.update digest (Format.sprintf "%.6f" (Unix.gettimeofday ())));
  List.sort !paths ~compare:Path.compare
  |> List.iter ~f:(fun path ->
    Md5.update digest (Path.to_string path);
    Md5.update digest (Utils.Cached_digest.file path);
    Md5.update digest "\x00");
  List.sort !env_vars ~compare:Partition.Env_var.compare
  |> List.iter ~f:(fun { Partition.Env_var. var; context } ->
    let env =
      match String.Map.find contexts context with
      | Some ctx -> ctx.Context.env
      | None -> Env.empty
    in
    Md5.update digest var;
    Md5.update digest "=";
    (match Env.get env var with
     | Some value ->
       Md5.update digest (Digest.string value)
     | None -> ());
    Md5.update digest "\x00");
  Md5.compute digest

(* Computes and saves digest for an SCC in partition graph, assuming that dependency
   digests are already computed. *)
let compute_component_digest t component ~projects ~file_tree ~contexts =
  let module Table = Partition.Table in
  let module Set = Partition.Set in
  let own_digest = compute_own_digest component ~projects ~file_tree ~contexts in
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
    let all_saved_deps =
      Table.find_or_add t.saved_deps partition ~f:(fun _ -> Set.empty)
    in
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
      let target_deps = Partition.Set.filter all_saved_deps ~f:(function
        | Partition.Target _ -> true
        | _ -> false)
      in
      let other_deps = Partition.Set.diff all_saved_deps target_deps in
      let target_deps_count = Partition.Set.cardinal target_deps in
      Format.eprintf "\t [deps: %!";
      Set.to_list other_deps
      |> List.map ~f:Partition.to_string_hum
      |> String.concat ~sep:", "
      |> prerr_string;
      if target_deps_count > 0 then
        Format.eprintf ", ...%d target deps" target_deps_count;
      Format.eprintf "]\n%!"
    end)

(* This function computes current digests for root_partition and all its
   direct or indirect dependencies. Digest for a partition is computed
   from its own files and digests of all dependencies. Circular dependencies
   between partitions are allowed.

   As soon as digest for a partition is computed, saved digest is removed if
   it's different from the newly computed digest.
*)
let compute_digests t root_partition ~projects ~file_tree ~contexts =
  (* This function implements Tarjan's strongly connected components (SCC) algorithm. *)
  let module Table = Partition.Table in
  let module Set = Partition.Set in
  let module Local = struct
    type info = {
      mutable index: int;
      mutable low_link: int;
      mutable on_stack: bool;
    }
  end in
  let open Local in
  let next_index = ref 0 in
  let stack = Stack.create () in
  let info = Table.create 64 in
  let iter_deps partition ~f =
    Option.iter (Table.find t.saved_deps partition) ~f:(fun deps -> Set.iter deps ~f)
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
             process it again. *)
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
        if Partition.equal w v then
          should_continue := false
      done;
      (* Tarjan's algorithm outputs SCCs in reverse topological sorting order, so
         we can be sure that all dependencies are already processed *)
      compute_component_digest t !component ~projects ~file_tree ~contexts
    end
  in
  iter_strong_components root_partition

let get_current_digest t partition ~projects ~file_tree ~contexts =
  match Partition.Table.find t.current_digests partition with
  | Some digest -> digest
  | None ->
    compute_digests t partition ~projects ~file_tree ~contexts;
    Partition.Table.find_exn t.current_digests partition

let is_clean t partition ~projects ~file_tree ~contexts =
  match Partition.Table.(
    find t.current_digests partition, find t.saved_digests partition) with
  | Some current_digest, Some saved_digest ->
    (* If current digest was computed that's different from saved digest, saved digest
       should be immediately removed. *)
    assert (current_digest = saved_digest);
    true
  | Some _, None ->
    false
  | None, _ ->
    let current_digest = get_current_digest t partition ~projects ~file_tree ~contexts in
    match Partition.Table.find t.saved_digests partition with
    | Some saved_digest ->
      current_digest = saved_digest
    | None -> false

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
