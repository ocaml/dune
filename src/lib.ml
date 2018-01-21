open Import

module Internal = struct
  type t = Path.t * Jbuild.Library.t
end

module T = struct
  type t =
    | Internal of Internal.t * bool
    | External of Findlib.package * bool

  let best_name = function
    | External (pkg, _) -> pkg.name
    | Internal ((_, lib), _) -> Jbuild.Library.best_name lib

  let compare a b = String.compare (best_name a) (best_name b)
end

include T
module Set = Set.Make(T)

let lib_obj_dir dir lib =
  Path.relative dir ("." ^ lib.Jbuild.Library.name ^ ".objs")

let dir = function
  | Internal ((dir, _), _) -> dir
  | External (pkg, _) -> pkg.dir

let obj_dir = function
  | Internal ((dir, lib), _) -> lib_obj_dir dir lib
  | External (pkg, _) -> pkg.dir

let included = function
  | Internal (_, inc) | External (_, inc) -> inc

let set_included = function
  | Internal (lib, _) -> Internal (lib, true)
  | External (pkg, _) -> External (pkg, true)

let unset_included = function
  | Internal (lib, _) -> Internal (lib, false)
  | External (pkg, _) -> External (pkg, false)

let include_paths ts =
  List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
    if included t then Path.Set.add (obj_dir t) acc else acc)

let include_flags ts =
  let dirs = include_paths ts in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let c_include_flags ts =
  let dirs =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      Path.Set.add (dir t) acc)
  in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let describe = function
  | Internal ((_, lib), _) ->
    sprintf "%s (local)"
      (match lib.public with
       | Some p -> p.name
       | None -> lib.name)
  | External (pkg, _) ->
    sprintf "%s (external)" pkg.name

let link_flags ts ~mode =
  Arg_spec.S
    (c_include_flags ts ::
     List.map ts ~f:(fun t ->
       match t with
       | External (pkg, _) ->
         Arg_spec.Deps (Mode.Dict.get pkg.archives mode)
       | Internal ((dir, lib), _) ->
         Dep (Path.relative dir (lib.name ^ Mode.compiled_lib_ext mode))))

let archive_files ts ~mode ~ext_lib =
  List.concat_map ts ~f:(function
    | External (pkg, _) ->
      Mode.Dict.get pkg.archives mode
    | Internal ((dir, lib), _) ->
      let l =
        [Path.relative dir (lib.name ^ Mode.compiled_lib_ext mode)]
      in
      if Jbuild.Library.has_stubs lib then
        Jbuild.Library.stubs_archive lib ~dir ~ext_lib :: l
      else
        l)

let jsoo_runtime_files ts =
  List.concat_map ts ~f:(function
    | External (pkg, _) ->
      List.map pkg.jsoo_runtime ~f:(Path.relative pkg.dir)
    | Internal ((dir, lib), _) ->
      List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir))
(*
let ppx_runtime_libraries ts =
  List.fold_left ts ~init:String_set.empty ~f:(fun acc t ->
    match t with
    | Internal (_, lib) ->
      String_set.union acc (String_set.of_list lib.ppx_runtime_libraries)
    | External pkg ->
      String_set.union acc (String_set.of_list pkg.ppx_runtime_deps))
*)

let rec reverse_and_propagate incs acc = function
  | [] -> acc
  | lib :: libs ->
    let lib =
      if included lib || not (String_set.mem (best_name lib) incs)
      then lib
      else set_included lib in
    reverse_and_propagate incs (lib :: acc) libs

let remove_dups_preserve_order libs =
  let rec loop incs seen libs acc =
    match libs with
    | [] -> reverse_and_propagate incs [] acc
    | lib :: libs ->
      let name = best_name lib in
      let incs = if included lib then String_set.add name incs else incs in
      if String_set.mem name seen then
        loop incs seen libs acc
      else
        loop incs (String_set.add name seen) libs (lib :: acc)
  in
  loop String_set.empty String_set.empty libs []
;;
