open Stdune

module Trace = struct
  type t = {
    sandbox_mode : Sandbox_mode.t;
    files : (string * Digest.t) list;
  }
end

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Alias of Alias.t
    | Glob of File_selector.t
    | Universe
    | Sandbox_config of Sandbox_config.t

  let env e = Env e
  let file f = File f
  let alias a = Alias a
  let universe = Universe
  let glob g = Glob g
  let sandbox_config config = Sandbox_config config

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, _ -> Lt
    | _, Env _ -> Gt
    | File x, File y -> Path.compare x y
    | File _, _ -> Lt
    | _, File _ -> Gt
    | Alias x, Alias y -> Alias.compare x y
    | Alias _, _ -> Lt
    | _, Alias _ -> Gt
    | Glob x, Glob y -> File_selector.compare x y
    | Glob _, _ -> Lt
    | _, Glob _ -> Gt
    | Universe, Universe -> Ordering.Eq
    | Universe, _ -> Lt
    | _, Universe -> Gt
    | Sandbox_config x, Sandbox_config y ->
      Sandbox_config.compare x y

  let unset = lazy (Digest.string "unset")

  let trace_file fn = (Path.to_string fn, Cached_digest.file fn)

  let trace t ~sandbox_mode ~env ~eval_pred =
    match t with
    | Universe -> ["universe", Digest.string "universe"]
    | File fn -> [trace_file fn]
    | Alias a -> [trace_file (Path.build (Alias.stamp_file a))]
    | Glob dir_glob ->
      eval_pred dir_glob
      |> Path.Set.to_list
      |> List.map ~f:trace_file
    | Env var ->
      let value =
        begin match Env.get env var with
        | None -> Lazy.force unset
        | Some v -> Digest.string v
        end
      in
      [var, value]
    | Sandbox_config config ->
      assert (Sandbox_config.mem config sandbox_mode);
      (* recorded globally for the whole dep set *)
      []

  let encode t =
    let open Dune_lang.Encoder in
    let sandbox_mode (mode : Sandbox_mode.t) =
      match mode with
      | None -> "none"
      | Some Copy -> "copy"
      | Some Symlink -> "symlink"
    in
    let sandbox_config (config : Sandbox_config.t) =
      list (fun x -> x) (
        List.filter_map Sandbox_mode.all ~f:(fun mode ->
          if not (Sandbox_config.mem config mode)
          then
            Some (pair string string ("disallow", sandbox_mode mode))
          else
            None))
    in
    match t with
    | Glob g -> pair string File_selector.encode ("glob", g)
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Dpath.encode ("File", f)
    | Alias a -> pair string Alias.encode ("Alias", a)
    | Universe -> string "Universe"
    | Sandbox_config config ->
      pair string sandbox_config ("Sandbox_config", config)

  let to_dyn _ = Dyn.opaque
end

include T

module O = Comparable.Make(T)

module Map = O.Map
module Set = struct
  include O.Set

  let has_universe t = mem t Universe

  let sandbox_config t =
    List.fold_left (to_list t) ~init:(Sandbox_config.no_special_requirements)
      ~f:(fun acc x -> match x with
        | Glob _ | Env _ | File _ | Alias _ | Universe -> acc
        | Sandbox_config config ->
          Sandbox_config.inter acc config)

  let of_files = List.fold_left ~init:empty ~f:(fun acc f -> add acc (file f))

  let of_files_set =
    Path.Set.fold ~init:empty ~f:(fun f acc -> add acc (file f))

  let trace t ~sandbox_mode ~env ~eval_pred =
    let files =
      List.concat_map (to_list t) ~f:(trace ~sandbox_mode ~env ~eval_pred)
    in
    { Trace.
      files;
      sandbox_mode;
    }

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let paths t ~eval_pred =
    fold t ~init:Path.Set.empty ~f:(fun d acc ->
      match d with
      | Alias a -> Path.Set.add acc (Path.build (Alias.stamp_file a))
      | File f -> Path.Set.add acc f
      | Glob g -> Path.Set.union acc (eval_pred g)
      | Universe
      | Env _ -> acc
      | Sandbox_config _ -> acc)

  let parallel_iter t ~f = Fiber.parallel_iter ~f (to_list t)

  let parallel_iter_files t ~f ~eval_pred =
    paths t ~eval_pred
    |> Path.Set.to_list
    |> Fiber.parallel_iter ~f

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
      match f with
      | Alias a -> Path.Set.add acc (Path.build (Alias.stamp_file_dir a))
      | Glob g -> Path.Set.add acc (File_selector.dir g)
      | File f -> Path.Set.add acc (Path.parent_exn f)
      | Universe
      | Env _ -> acc
      | Sandbox_config _ -> acc)
end

type eval_pred = File_selector.t -> Path.Set.t
