open! Stdune
open Import

type static = Static
type dynamic = Dynamic

type ('a, _) t =
  | A        : string -> ('a, _) t
  | As       : string list -> ('a, _) t
  | S        : ('a, 'b) t list -> ('a, 'b) t
  | Concat   : string * ('a, 'b) t list  -> ('a, 'b) t
  | Dep      : Path.t -> ('a, _) t
  | Deps     : Path.t list -> ('a, _) t
  | Target   : Path.t -> ('a, dynamic) t
  | Path     : Path.t -> ('a, _) t
  | Paths    : Path.t list -> ('a, _) t
  | Hidden_deps    : Dep.Set.t -> ('a, _) t
  | Hidden_targets : Path.t list -> ('a, dynamic) t
  | Dyn      : ('a -> (Nothing.t, static) t) -> ('a, dynamic) t
  | Fail     : fail -> ('a, _) t

let static_deps =
  let rec add_deps ts set =
    List.fold_left ts ~init:set ~f:(fun set t ->
      match t with
      | Dep fn -> Dep.Set.add set (Dep.file fn)
      | Deps        fns -> Dep.Set.union set (Dep.Set.of_files fns)
      | Hidden_deps deps -> Dep.Set.union set deps
      | S ts
      | Concat (_, ts) -> add_deps ts set
      | _ -> set)
  in
  fun ts -> add_deps ts Dep.Set.empty

let rec add_targets ts acc =
  List.fold_left ts ~init:acc ~f:(fun acc t ->
    match t with
    | Target fn  -> fn :: acc
    | Hidden_targets fns -> List.rev_append fns acc
    | S ts
    | Concat (_, ts) -> add_targets ts acc
    | _ -> acc)

let expand ~dir ts x =
  let dyn_deps = ref Dep.Set.empty in
  let add_dep path = dyn_deps := Dep.Set.add !dyn_deps (Dep.file path) in
  let rec loop_dyn : (Nothing.t, static) t -> string list = function
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
    | Hidden_deps l ->
      dyn_deps := Dep.Set.union !dyn_deps l;
      []
    | Fail f -> f.fail ()
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
    | Fail f -> f.fail ()
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

let fail e = Fail { fail = fun _ -> raise e }

let of_result = function
  | Ok x -> x
  | Error e -> fail e

let of_result_map res ~f =
  match res with
  | Ok    x -> f x
  | Error e -> fail e
