open! Stdune
open Import

type 'a t =
  | A        of string
  | As       of string list
  | S        of 'a t list
  | Concat   of string * 'a t list
  | Dep      of Path.t
  | Deps     of Path.t list
  | Target   of Path.t
  | Path     of Path.t
  | Paths    of Path.t list
  | Hidden_deps    of Path.t list
  | Hidden_targets of Path.t list
  | Dyn      of ('a -> nothing t)

let rec add_deps ts set =
  List.fold_left ts ~init:set ~f:(fun set t ->
    match t with
    | Dep fn -> Path.Set.add set fn
    | Deps        fns
    | Hidden_deps fns -> Path.Set.union set (Path.Set.of_list fns)
    | S ts
    | Concat (_, ts) -> add_deps ts set
    | _ -> set)

let rec add_targets ts acc =
  List.fold_left ts ~init:acc ~f:(fun acc t ->
    match t with
    | Target fn  -> fn :: acc
    | Hidden_targets fns -> List.rev_append fns acc
    | S ts
    | Concat (_, ts) -> add_targets ts acc
    | _ -> acc)

let expand ~dir ts x =
  let dyn_deps = ref Path.Set.empty in
  let add_dep path = dyn_deps := Path.Set.add !dyn_deps path in
  let rec loop_dyn : nothing t -> string list = function
    | A s  -> [s]
    | As l -> l
    | Dep fn ->
      add_dep fn;
      [Path.reach fn ~from:dir]
    | Path fn -> [Path.reach fn ~from:dir]
    | Deps fns ->
      List.map fns ~f:(fun fn ->
        add_dep fn;
        Path.reach ~from:dir fn)
    | Paths fns ->
      List.map fns ~f:(Path.reach ~from:dir)
    | S ts -> List.concat_map ts ~f:loop_dyn
    | Concat (sep, ts) -> [String.concat ~sep (loop_dyn (S ts))]
    | Target _ | Hidden_targets _ -> die "Target not allowed under Dyn"
    | Dyn _ -> assert false
    | Hidden_deps l ->
      dyn_deps := Path.Set.union !dyn_deps (Path.Set.of_list l);
      []
  in
  let rec loop = function
    | A s  -> [s]
    | As l -> l
    | (Dep fn | Path fn) -> [Path.reach fn ~from:dir]
    | (Deps fns | Paths fns) -> List.map fns ~f:(Path.reach ~from:dir)
    | S ts -> List.concat_map ts ~f:loop
    | Concat (sep, ts) -> [String.concat ~sep (loop (S ts))]
    | Target fn -> [Path.reach fn ~from:dir]
    | Dyn f -> loop_dyn (f x)
    | Hidden_deps _ | Hidden_targets _ -> []
  in
  let l = List.concat_map ts ~f:loop in
  (l, !dyn_deps)

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)

let of_result = function
  | Ok x -> x
  | Error e -> Dyn (fun _ -> raise e)

let of_result_map res ~f =
  match res with
  | Ok    x -> f x
  | Error e -> Dyn (fun _ -> raise e)

module Simple = struct
  type t =
    | A     of string
    | As    of string list
    | S     of t list
    | Dep   of Path.t
    | Deps  of Path.t list
    | Path  of Path.t
    | Paths of Path.t list

  let deps =
    let rec loop acc t =
      match t with
      | A _ | As _ | Path _ | Paths _ -> acc
      | S l -> List.fold_left l ~init:acc ~f:loop
      | Dep fn -> Path.Set.add acc fn
      | Deps fns -> Path.Set.union acc (Path.Set.of_list fns)
    in
    fun t -> loop Path.Set.empty t

  let expand t ~dir =
    let rec loop = function
      | A s -> [s]
      | As l -> l
      | (Dep fn | Path fn) -> [Path.reach fn ~from:dir]
      | (Deps fns | Paths fns) -> List.map fns ~f:(Path.reach ~from:dir)
      | S ts -> List.concat_map ts ~f:loop
    in
    loop t
end

let rec of_simple : Simple.t -> _ t = function
  | A x -> A x
  | As x -> As x
  | S l -> S (List.map l ~f:of_simple)
  | Dep x -> Dep x
  | Deps x -> Deps x
  | Path x -> Path x
  | Paths x -> Paths x
