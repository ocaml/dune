open Stdune

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t

  let env e = Env e
  let file f = File f

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, File _ -> Ordering.Lt
    | File x, File y -> Path.compare x y
    | File _, Env _ -> Ordering.Gt

  let unset = lazy (Digest.string "unset")

  let trace t ~env =
    match t with
    | File fn ->
      (Path.to_string fn, Utils.Cached_digest.file fn)
    | Env var ->
      let value =
        begin match Env.get env var with
        | None -> Lazy.force unset
        | Some v -> Digest.string v
        end
      in
      (var, value)

  let pp fmt = function
    | Env e -> Format.fprintf fmt "Env %S" e
    | File f -> Format.fprintf fmt "File %a" Path.pp f

  let encode t =
    let open Dune_lang.Encoder in
    match t with
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Path_dune_lang.encode ("File", f)
end

include T

module Set = struct
  include Set.Make(T)

  let trace t ~env = List.map ~f:(trace ~env) (to_list t)

  let pp fmt (t : t) =
    Format.fprintf fmt "Deps %a" (Fmt.list pp) (to_list t)

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let file_list t =
    to_list t
    |> List.filter_map ~f:(function
      | File f -> Some f
      | Env _ -> None)

  let paths t = Path.Set.of_list (file_list t)

  let parallel_iter t ~f = Fiber.parallel_iter ~f (file_list t)

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
      match f with
      | File f -> Path.Set.add acc (Path.parent_exn f)
      | Env _ -> acc)
end
