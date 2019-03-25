open Stdune

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Glob of Path.t * Path.t Predicate.t
    | Universe

  let env e = Env e
  let file f = File f
  let universe = Universe
  let glob ~dir predicate = Glob (dir, predicate)

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, _ -> Lt
    | _, Env _ -> Gt
    | File x, File y -> Path.compare x y
    | File _, _ -> Lt
    | _, File _ -> Gt
    | Glob (d1, p1), Glob (d2, p2) ->
      Tuple.T2.compare Path.compare Predicate.compare (d1, p1) (d2, p2)
    | Glob _, _ -> Lt
    | _, Glob _ -> Gt
    | Universe, Universe -> Ordering.Eq

  let unset = lazy (Digest.string "unset")

  let trace_file fn = (Path.to_string fn, Utils.Cached_digest.file fn)

  let trace t ~env ~eval_pred =
    match t with
    | Universe -> ["universe", Digest.string "universe"]
    | File fn -> [trace_file fn]
    | Glob (dir, pred) ->
      eval_pred ~dir pred
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

  let pp fmt = function
    | Env e -> Format.fprintf fmt "Env %S" e
    | File f -> Format.fprintf fmt "File %a" Path.pp f
    | Glob (dir, pred) ->
      Format.fprintf fmt "Glob (%a, %a)"
        Path.pp dir
        Predicate.pp pred
    | Universe -> Format.fprintf fmt "Universe"

  let encode t =
    let open Dune_lang.Encoder in
    match t with
    | Glob (dir, pred) ->
      triple
        string Path_dune_lang.encode Predicate.encode
        ("glob", dir, pred)
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Path_dune_lang.encode ("File", f)
    | Universe -> string "Universe"
end

include T

module Set = struct
  include Set.Make(T)

  let has_universe t = mem t Universe

  let of_files = List.fold_left ~init:empty ~f:(fun acc f -> add acc (file f))

  let trace t ~env ~eval_pred =
    List.concat_map (to_list t) ~f:(trace ~env ~eval_pred)

  let pp fmt (t : t) =
    Format.fprintf fmt "Deps %a" (Fmt.list pp) (to_list t)

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let paths t ~eval_pred =
    fold t ~init:Path.Set.empty ~f:(fun d acc ->
      match d with
      | File f -> Path.Set.add acc f
      | Glob (dir, pred) -> Path.Set.union acc (eval_pred ~dir pred)
      | Universe
      | Env _ -> acc)

  let parallel_iter t ~f = Fiber.parallel_iter ~f (to_list t)

  let parallel_iter_files t ~f ~eval_pred =
    paths t ~eval_pred
    |> Path.Set.to_list
    |> Fiber.parallel_iter ~f

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
      match f with
      | Glob (dir, _) -> Path.Set.add acc dir
      | File f -> Path.Set.add acc (Path.parent_exn f)
      | Universe
      | Env _ -> acc)
end

type eval_pred = dir:Path.t -> Path.t Predicate.t -> Path.Set.t
