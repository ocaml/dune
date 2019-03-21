open Stdune

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Glob of Path.t * string Predicate.t
    | Universe

  let env e = Env e
  let file f = File f
  let universe = Universe
  let glob ~dir predicate = Glob (dir, predicate)

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, File _
    | Env _, Glob _
    | Env _, Universe -> Ordering.Lt
    | File x, File y -> Path.compare x y
    | File _, Glob _
    | File _, Env _ -> Ordering.Gt
    | File _, Universe -> Ordering.Lt
    | Glob (d1, p1), Glob (d2, p2) ->
      Tuple.T2.compare Path.compare Predicate.compare (d1, p1) (d2, p2)
    | Glob _, (Env _ | File _) -> Ordering.Gt
    | Glob _, Universe -> Ordering.Lt
    | Universe, Universe -> Ordering.Eq
    | Universe, (Env _ | File _ | Glob _) -> Gt

  let unset = lazy (Digest.string "unset")

  let trace_file fn = (Path.to_string fn, Utils.Cached_digest.file fn)

  let trace t ~env ~eval_pred =
    match t with
    | Universe -> ["universe", Digest.string "universe"]
    | File fn -> [trace_file fn]
    | Glob (dir, pred) ->
      eval_pred ~dir pred
      |> List.map ~f:(fun f -> trace_file (Path.relative dir f))
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

  let trace t ~env ~eval_pred =
    List.concat_map (to_list t) ~f:(trace ~env ~eval_pred)

  let pp fmt (t : t) =
    Format.fprintf fmt "Deps %a" (Fmt.list pp) (to_list t)

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let file_list t ~eval_pred =
    to_list t
    |> List.concat_map ~f:(function
      | File f -> [f]
      | Glob (dir, pred) ->
        eval_pred ~dir pred
        |> List.map ~f:(Path.relative dir)
      | Universe
      | Env _ -> [])

  let paths t ~eval_pred = Path.Set.of_list (file_list t ~eval_pred)

  let parallel_iter t ~f = Fiber.parallel_iter ~f (to_list t)

  let parallel_iter_files t ~f ~eval_pred =
    Fiber.parallel_iter ~f (file_list t ~eval_pred)

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
      match f with
      | Glob (dir, _) -> Path.Set.add acc dir
      | File f -> Path.Set.add acc (Path.parent_exn f)
      | Universe
      | Env _ -> acc)
end
