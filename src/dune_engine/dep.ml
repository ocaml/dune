open Stdune

let eval_pred = Fdecl.create Dyn.Encoder.opaque

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

  let to_dyn t = Dyn.String (Dune_lang.to_string (encode t))
end

include T
module O = Comparable.Make (T)

module Map = struct
  include O.Map
  include Memo.Build.Make_map_traversals (O.Map)

  let sandbox_config t =
    foldi t ~init:Sandbox_config.no_special_requirements ~f:(fun x _ acc ->
        match x with
        | File_selector _
        | Env _
        | File _
        | Alias _
        | Universe ->
          acc
        | Sandbox_config config -> Sandbox_config.inter acc config)

  let has_universe t = mem t Universe
end

module Fact = struct
  type t =
    | Nothing
    | File of Path.t * Digest.t
    | File_selector of Dyn.t * Digest.t Path.Map.t
    | Alias of Digest.t Path.Map.t

  module Stable_for_digest = struct
    type file = string * Digest.t

    type t =
      | Env of string * string option
      | File of file
      | File_selector of Dyn.t * file list
      | Alias of file list
  end

  let nothing = Nothing

  let file fn digest = File (fn, digest)

  let file_selector fs digests =
    let id = File_selector.to_dyn fs in
    File_selector (id, digests)

  let alias _alias files = Alias files
end

module Facts = struct
  type t = Fact.t Map.t

  let empty = Map.empty

  let union a b =
    Map.union a b ~f:(fun _ a b ->
        assert (a = b);
        Some a)

  let paths t =
    Map.fold t ~init:Path.Map.empty ~f:(fun fact acc ->
        match (fact : Fact.t) with
        | Nothing -> acc
        | File (p, d) -> Path.Map.set acc p d
        | File_selector (_, ps)
        | Alias ps ->
          Path.Map.union acc ps ~f:(fun _ a _ -> Some a))

  let dirs t =
    Map.fold t ~init:Path.Set.empty ~f:(fun fact acc ->
        match (fact : Fact.t) with
        | Nothing -> acc
        | File (p, _) -> Path.Set.add acc (Path.parent_exn p)
        | File_selector (_, ps)
        | Alias ps ->
          Path.Set.union acc
            (Path.Map.to_list_map ps ~f:(fun x _ -> Path.parent_exn x)
            |> Path.Set.of_list))

  let digest t ~sandbox_mode ~env =
    let facts =
      let file (p, d) = (Path.to_string p, d) in
      Map.foldi t ~init:[]
        ~f:(fun dep fact acc : Fact.Stable_for_digest.t list ->
          match dep with
          | Env var -> Env (var, Env.get env var) :: acc
          | Universe -> acc
          | Sandbox_config config ->
            assert (Sandbox_config.mem config sandbox_mode);
            (* recorded globally for the whole dep set, see bellow *)
            acc
          | File _
          | File_selector _
          | Alias _ -> (
            match (fact : Fact.t) with
            | Nothing -> acc
            | File (p, d) -> File (file (p, d)) :: acc
            | File_selector (id, ps) ->
              File_selector
                ( id
                , Path.Map.to_list_map ps ~f:(fun p d -> (Path.to_string p, d))
                )
              :: acc
            | Alias ps ->
              Alias
                (Path.Map.to_list_map ps ~f:(fun p d -> (Path.to_string p, d)))
              :: acc))
    in
    Digest.generic (sandbox_mode, facts)
end

module Set = struct
  include O.Map

  type t = unit Map.t

  let singleton dep = singleton dep ()

  let add t x = add_exn t x ()

  let of_list = Map.of_list_unit

  let of_list_map l ~f = Map.of_list_unit (List.map l ~f:(fun x -> f x))

  let to_list = keys

  let of_files = List.fold_left ~init:empty ~f:(fun acc f -> add acc (file f))

  let of_files_set =
    Path.Set.fold ~init:empty ~f:(fun f acc -> add acc (file f))

  let union = union ~f:(fun _ () () -> Some ())

  let union_map l ~f =
    List.fold_left ~init:empty l ~f:(fun acc x ->
        let s = f x in
        union acc s)

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let static_paths t =
    foldi t ~init:(Path.Set.empty, [])
      ~f:(fun d _ ((acc_paths, acc_aliases) as acc) ->
        match d with
        | Alias a -> (acc_paths, a :: acc_aliases)
        | File f -> (Path.Set.add acc_paths f, acc_aliases)
        | File_selector g ->
          (Path.Set.union acc_paths (Fdecl.get eval_pred g), acc_aliases)
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

  let source_tree dir =
    let t = source_tree dir in
    (t, fst (static_paths t))

  let files_approx t = static_paths t |> fst

  let parallel_iter_files_approx t ~f =
    static_paths t |> fst |> Memo.Build.parallel_iter_set (module Path.Set) ~f
end
