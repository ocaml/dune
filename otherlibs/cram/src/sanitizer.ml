open Stdune
module Re = Dune_re

type t =
  | Id
  | Rewrite_build_path_prefix_map
  | Apply of (string -> string)
  | Seq of t * t

let id = Id

let make f = Apply f

let rewrite_paths =
  let var = "BUILD_PATH_PREFIX_MAP" in
  match Sys.getenv var with
  | exception Not_found -> fun s -> s
  | s -> (
    match Build_path_prefix_map.decode_map s with
    | Error msg ->
      Printf.eprintf "Cannot decode %s: %s\n%!" var msg;
      exit 2
    | Ok map ->
      let abs_path_re =
        let not_dir = Printf.sprintf " \n\r\t%c" Bin.path_sep in
        Re.(compile (seq [ char '/'; rep1 (diff any (set not_dir)) ]))
      in
      fun s ->
        Re.replace abs_path_re s ~f:(fun g ->
            Build_path_prefix_map.rewrite map (Re.Group.get g 0)) )

let rewrite_build_path_prefix_map = Rewrite_build_path_prefix_map

let rec apply t s =
  match t with
  | Id -> s
  | Rewrite_build_path_prefix_map -> rewrite_paths s
  | Apply f -> f s
  | Seq (a, b) -> apply b (apply a s)

module O = struct
  let ( >>> ) x y = Seq (x, y)
end
