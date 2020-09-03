open Stdune

module Trace = struct
  module Fact = struct
    type t =
      | Env of string * string option
      | File of (string * Digest.t)
      | File_selector of Dyn.t * (string * Digest.t) list
  end

  type t =
    { sandbox_mode : Sandbox_mode.t
    ; facts : Fact.t list
    }
end

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Alias of Alias.t
    | File_selector of File_selector.t
    | Universe
    | Sandbox_config of Sandbox_config.t

  let env e = Env e

  let file f = File f

  let alias a = Alias a

  let universe = Universe

  let file_selector g = File_selector g

  let sandbox_config config = Sandbox_config config

  let compare x y =
    match (x, y) with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, _ -> Lt
    | _, Env _ -> Gt
    | File x, File y -> Path.compare x y
    | File _, _ -> Lt
    | _, File _ -> Gt
    | Alias x, Alias y -> Alias.compare x y
    | Alias _, _ -> Lt
    | _, Alias _ -> Gt
    | File_selector x, File_selector y -> File_selector.compare x y
    | File_selector _, _ -> Lt
    | _, File_selector _ -> Gt
    | Universe, Universe -> Ordering.Eq
    | Universe, _ -> Lt
    | _, Universe -> Gt
    | Sandbox_config x, Sandbox_config y -> Sandbox_config.compare x y

  let trace_file fn = (Path.to_string fn, Cached_digest.file fn)

  let trace t ~sandbox_mode ~env ~eval_pred : Trace.Fact.t Option.t =
    match t with
    | Env var -> Some (Env (var, Env.get env var))
    | File fn -> Some (File (trace_file fn))
    | Alias a -> Some (File (trace_file (Path.build (Alias.stamp_file a))))
    | File_selector dir_glob ->
      let id = File_selector.to_dyn dir_glob
      and files =
        eval_pred dir_glob |> Path.Set.to_list |> List.map ~f:trace_file
      in
      Some (File_selector (id, files))
    | Universe -> None
    | Sandbox_config config ->
      assert (Sandbox_config.mem config sandbox_mode);
      (* recorded globally for the whole dep set *)
      None

  let encode t =
    let open Dune_lang.Encoder in
    let sandbox_mode (mode : Sandbox_mode.t) =
      match mode with
      | None -> "none"
      | Some Copy -> "copy"
      | Some Symlink -> "symlink"
    in
    let sandbox_config (config : Sandbox_config.t) =
      list
        (fun x -> x)
        (List.filter_map Sandbox_mode.all ~f:(fun mode ->
             if not (Sandbox_config.mem config mode) then
               Some (pair string string ("disallow", sandbox_mode mode))
             else
               None))
    in
    match t with
    | File_selector g -> pair string File_selector.encode ("glob", g)
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Dpath.encode ("File", f)
    | Alias a -> pair string Alias.encode ("Alias", a)
    | Universe -> string "Universe"
    | Sandbox_config config ->
      pair string sandbox_config ("Sandbox_config", config)

  let to_dyn _ = Dyn.opaque
end

include T
module O = Comparable.Make (T)
module Map = O.Map

module Set = struct
  include O.Set

  let has_universe t = mem t Universe

  let sandbox_config t =
    List.fold_left (to_list t) ~init:Sandbox_config.no_special_requirements
      ~f:(fun acc x ->
        match x with
        | File_selector _
        | Env _
        | File _
        | Alias _
        | Universe ->
          acc
        | Sandbox_config config -> Sandbox_config.inter acc config)

  let of_files = List.fold_left ~init:empty ~f:(fun acc f -> add acc (file f))

  let of_files_set =
    Path.Set.fold ~init:empty ~f:(fun f acc -> add acc (file f))

  let trace t ~sandbox_mode ~env ~eval_pred =
    let facts =
      List.filter_map (to_list t) ~f:(trace ~sandbox_mode ~env ~eval_pred)
    in
    { Trace.facts; sandbox_mode }

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let paths t ~eval_pred =
    fold t ~init:Path.Set.empty ~f:(fun d acc ->
        match d with
        | Alias a -> Path.Set.add acc (Path.build (Alias.stamp_file a))
        | File f -> Path.Set.add acc f
        | File_selector g -> Path.Set.union acc (eval_pred g)
        | Universe
        | Env _ ->
          acc
        | Sandbox_config _ -> acc)

  let parallel_iter t ~f = Fiber.parallel_iter ~f (to_list t)

  let parallel_iter_files t ~f ~eval_pred =
    paths t ~eval_pred |> Path.Set.to_list |> Fiber.parallel_iter ~f

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
        match f with
        | Alias a -> Path.Set.add acc (Path.build (Alias.stamp_file_dir a))
        | File_selector g -> Path.Set.add acc (File_selector.dir g)
        | File f -> Path.Set.add acc (Path.parent_exn f)
        | Universe
        | Env _ ->
          acc
        | Sandbox_config _ -> acc)

  (* This is to force the rules to be loaded for directories without files when
     depending on [(source_tree x)]. Otherwise, we wouldn't clean up stale
     directories in directories that contain no file. *)
  let dir_without_files_dep dir =
    file_selector (File_selector.create ~dir Predicate.false_)

  let source_tree dir =
    let prefix_with, dir = Path.extract_build_context_dir_exn dir in
    match File_tree.find_dir dir with
    | None -> empty
    | Some dir ->
      File_tree.Dir.fold dir ~init:empty ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir acc ->
          let files = File_tree.Dir.files dir in
          let path = Path.append_source prefix_with (File_tree.Dir.path dir) in
          match String.Set.is_empty files with
          | true -> add acc (dir_without_files_dep path)
          | false ->
            let paths =
              String.Set.fold files ~init:Path.Set.empty ~f:(fun fn acc ->
                  Path.Set.add acc (Path.relative path fn))
            in
            add_paths acc paths)
end

type eval_pred = File_selector.t -> Path.Set.t
