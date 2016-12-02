open Import

module T = struct
  type t =
    | Internal of Path.t * Jbuild_types.Library.t
    | External of Findlib.package

  let best_name = function
    | External pkg -> pkg.name
    | Internal (_, lib) -> Option.value lib.public_name ~default:lib.name

  let compare a b = String.compare (best_name a) (best_name b)
end

include T
module Set = Set.Make(T)

let deps = function
  | Internal (_, lib) -> lib.libraries
  | External pkg -> pkg.requires

let dir = function
  | Internal (dir, _) -> dir
  | External pkg -> pkg.dir

let include_flags ts =
  let dirs =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      Path.Set.add (dir t) acc)
  in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let describe = function
  | Internal (_, lib) ->
    sprintf "%s (local)" (Option.value lib.public_name ~default:lib.name)
  | External pkg ->
    sprintf "%s (external)" pkg.name

let link_flags ts ~mode =
  Arg_spec.S
    (include_flags ts ::
     List.map ts ~f:(fun t : _ Arg_spec.t ->
       match t with
       | External pkg ->
         Deps_rel (pkg.dir, Mode.Dict.get pkg.archives mode)
       | Internal (dir, lib) ->
         Dep_rel (dir, lib.name ^ Mode.compiled_lib_ext mode)))

let archive_files ts ~mode =
  List.concat_map ts ~f:(function
    | External pkg ->
      List.map (Mode.Dict.get pkg.archives mode) ~f:(Path.relative pkg.dir)
    | Internal (dir, lib) ->
      [Path.relative dir (lib.name ^ Mode.compiled_lib_ext mode)])

let ppx_runtime_libraries ts =
  List.fold_left ts ~init:String_set.empty ~f:(fun acc t ->
    match t with
    | Internal (_, lib) ->
      String_set.union acc (String_set.of_list lib.ppx_runtime_libraries)
    | External pkg ->
      String_set.union acc (String_set.of_list pkg.ppx_runtime_deps))
