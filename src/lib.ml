open Import

module FP = Findlib.Package

module Internal = struct
  type t = Path.t * Jbuild.Library.t
end

module T = struct
  type t =
    | Internal of Internal.t
    | External of FP.t

  let internal i = Internal i
  let external_ i = External i

  let best_name = function
    | External pkg -> FP.name pkg
    | Internal (_, lib) -> Jbuild.Library.best_name lib

  let compare a b = String.compare (best_name a) (best_name b)
end

include T
module Set = Set.Make(T)

let lib_obj_dir dir lib =
  Path.relative dir ("." ^ lib.Jbuild.Library.name ^ ".objs")

let get_internal = function
  | Internal x -> Some x
  | External _ -> None

let to_either = function
  | Internal x -> Inl x
  | External x -> Inr x

let src_dir = function
  | External _ -> None
  | Internal (dir, _) -> Some dir

let obj_dir = function
  | External pkg -> FP.dir pkg
  | Internal (dir, lib) -> lib_obj_dir dir lib

let src_or_obj_dir t =
  match src_dir t with
  | None -> obj_dir t
  | Some dir -> dir

let is_local lib = Path.is_local (obj_dir lib)

let include_paths ts ~stdlib_dir =
  List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
    Path.Set.add (obj_dir t) acc)
  |> Path.Set.remove stdlib_dir

let include_flags ts ~stdlib_dir =
  let dirs = include_paths ts ~stdlib_dir in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let c_include_flags ts ~stdlib_dir =
  let dirs =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      Path.Set.add (src_or_obj_dir t) acc)
    |> Path.Set.remove stdlib_dir
  in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let describe = function
  | Internal (_, lib) ->
    sprintf "%s (local)"
      (match lib.public with
       | Some p -> p.name
       | None -> lib.name)
  | External pkg ->
    sprintf "%s (external)" (FP.name pkg)

let link_flags ts ~mode ~stdlib_dir =
  Arg_spec.S
    (c_include_flags ts ~stdlib_dir ::
     List.map ts ~f:(fun t ->
       match t with
       | External pkg ->
         Arg_spec.Deps (FP.archives pkg mode)
       | Internal (dir, lib) ->
         Dep (Path.relative dir (lib.name ^ Mode.compiled_lib_ext mode))))

let stub_archives t ~ext_lib =
  match t with
  | External _ -> None
  | Internal (dir, lib) ->
    if Jbuild.Library.has_stubs lib then
      Some (Jbuild.Library.stubs_archive lib ~dir ~ext_lib)
    else
      None

let ml_archives t ~mode ~ext_lib =
  match t with
  | External pkg -> FP.archives pkg mode
  | Internal (dir, lib) ->
    let l =
      [Path.relative dir (lib.name ^ Mode.compiled_lib_ext mode)]
    in
    match mode, ext_lib with
    | Byte, _
    | Native, None -> l
    | Native, Some ext_lib -> Path.relative dir (lib.name ^ ext_lib) :: l

let archive_files ts ~mode ~ext_lib =
  List.concat_map ts ~f:(fun lib ->
    ml_archives lib ~mode ~ext_lib:(Some ext_lib) @
    Option.to_list (stub_archives lib ~ext_lib))

let jsoo_archives t =
  ml_archives t ~mode:Mode.Byte ~ext_lib:None
  |> List.map ~f:(Path.extend_basename ~suffix:".js")

let jsoo_runtime_files ts =
  List.concat_map ts ~f:(function
    | External pkg ->
      List.map (FP.jsoo_runtime pkg) ~f:(Path.relative (FP.dir pkg))
    | Internal (dir, lib) ->
      List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir))

let ppx_runtime_libraries t =
  String_set.of_list (
    match t with
    | Internal (_, lib) -> lib.ppx_runtime_libraries
    | External pkg -> List.map ~f:FP.name (FP.ppx_runtime_deps pkg)
  )

let requires = function
  | Internal (_, lib) ->
    lib.buildable.libraries
  | External pkg ->
    List.map ~f:(fun fp -> Jbuild.Lib_dep.direct (FP.name fp)) (FP.requires pkg)

let scope = function
  | Internal (dir, _) -> `Dir dir
  | External _ -> `External

let remove_dups_preserve_order libs =
  let rec loop seen libs acc =
    match libs with
    | [] -> List.rev acc
    | lib :: libs ->
      let name = best_name lib in
      if String_set.mem name seen then
        loop seen libs acc
      else
        loop (String_set.add name seen) libs (lib :: acc)
  in
  loop String_set.empty libs []
;;

let public_name = function
  | External pkg -> Some (FP.name pkg)
  | Internal (_, lib) -> Option.map lib.public ~f:(fun p -> p.name)

let unique_id = function
  | External pkg -> FP.name pkg
  | Internal (dir, lib) ->
    match lib.public with
    | Some p -> p.name
    | None -> Path.to_string dir ^ "\000" ^ lib.name

type local =
  { src: Path.t
  ; name: string
  }

let local = function
  | Internal (dir, lib) -> Some { src = dir; name = lib.name }
  | External _ -> None

let exists_name t ~f =
  match t with
  | External pkg -> f (FP.name pkg)
  | Internal (_, lib) ->
    (f lib.name) || (
      match lib.public with
      | None -> false
      | Some p -> f p.name
    )
