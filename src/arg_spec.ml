open Import

module Pset = Path.Set

type 'a t =
  | A        of string
  | As       of string list
  | S        of 'a t list
  | Dep      of Path.t
  | Deps     of Path.t list
  | Dep_rel  of Path.t * string
  | Deps_rel of Path.t * string list
  | Path     of Path.t
  | Paths    of Path.t list
  | Dyn      of ('a -> nothing t)

let rec add_deps ts set =
  List.fold_left ts ~init:set ~f:(fun set t ->
    match t with
    | Dep  fn  -> Pset.add fn set
    | Deps fns -> Pset.union set (Pset.of_list fns)
    | Dep_rel  (dir, fn) -> Pset.add (Path.relative dir fn) set
    | Deps_rel (dir, fns) ->
      List.fold_left fns ~init:set ~f:(fun set fn ->
        Pset.add (Path.relative dir fn) set)
    | S ts -> add_deps ts set
    | _ -> set)

let expand ~dir ts x =
  let dyn_deps = ref Path.Set.empty in
  let add_dep path = dyn_deps := Path.Set.add path !dyn_deps in
  let rec loop_dyn : nothing t -> string list = function
    | A s  -> [s]
    | As l -> l
    | Dep_rel (dir, fn) ->
      add_dep (Path.relative dir fn);
      [fn]
    | Deps_rel (dir, fns) ->
      List.iter fns ~f:(fun fn -> add_dep (Path.relative dir fn));
      fns
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
    | Dyn _ -> assert false
  in
  let rec loop = function
    | A s  -> [s]
    | As l -> l
    | Dep_rel (_, fn) -> [fn]
    | Deps_rel (_, fns) -> fns
    | (Dep fn | Path fn) -> [Path.reach fn ~from:dir]
    | (Deps fns | Paths fns) -> List.map fns ~f:(Path.reach ~from:dir)
    | S ts -> List.concat_map ts ~f:loop
    | Dyn f -> loop_dyn (f x)
  in
  let l = List.concat_map ts ~f:loop in
  (l, !dyn_deps)
