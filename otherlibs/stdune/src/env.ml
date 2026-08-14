module Sys = Stdlib.Sys

module Var = struct
  module T = struct
    type t = string

    let compare =
      if Sys.win32
      then fun a b -> String.compare (String.lowercase a) (String.lowercase b)
      else String.compare
    ;;

    let hash =
      if Sys.win32 then fun var -> String.hash (String.lowercase var) else String.hash
    ;;

    let to_dyn = Dyn.string
  end

  include Comparable.Make (T)
  include T

  let of_string s =
    if String.contains s '\000'
    then
      Code_error.raise
        "Env.Var.of_string: NUL byte in environment variable name"
        [ "name", Dyn.string s ];
    s
  ;;

  let to_string t = t
  let repr = Repr.view Repr.string ~to_:to_string
  let temp_dir = of_string (if Sys.win32 then "TEMP" else "TMPDIR")
  let _PATH = of_string "PATH"
  let _OCAMLPARAM = of_string "OCAMLPARAM"
  let _OCAMLFIND_CONF = of_string "OCAMLFIND_CONF"
  let _INSIDE_EMACS = of_string "INSIDE_EMACS"
  let _LC_ALL = of_string "LC_ALL"
  let _GIT_DIR = of_string "GIT_DIR"
  let _XDG_CACHE_HOME = of_string "XDG_CACHE_HOME"
  let _DUNE_ACTION_TRACE_DIR = of_string "DUNE_ACTION_TRACE_DIR"
end

module Set = Var.Set
module Map = Var.Map

module Value : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_string value =
    if String.contains value '\000'
    then
      Code_error.raise
        "Env: NUL byte in environment variable value"
        [ "value", Dyn.string value ];
    value
  ;;

  let to_string t = t
end

type t = Value.t Map.t

let equal =
  Map.equal ~equal:(fun x y -> String.equal (Value.to_string x) (Value.to_string y))
;;

let hash t =
  Map.foldi t ~init:(Hash.create ()) ~f:(fun var value acc ->
    let acc = Hash.feed acc (Var.hash var) in
    Hash.feed acc (String.hash (Value.to_string value)))
  |> Hash.hash
;;

let of_map vars = Map.map vars ~f:Value.of_string
let empty = Map.empty
let is_empty = Map.is_empty
let vars = Set.of_keys
let get t var = Option.map (Map.find t var) ~f:Value.to_string

let to_list t =
  Map.foldi t ~init:[] ~f:(fun var value acc -> (var, Value.to_string value) :: acc)
;;

let render (var, value) = String.append_with_char (Var.to_string var) ~sep:'=' value
let to_unix t = List.map (to_list t) ~f:render

let to_windows_block t =
  match to_unix t with
  | [] -> "\000\000"
  | env ->
    let len = List.fold_left env ~init:1 ~f:(fun acc s -> acc + String.length s + 1) in
    let block = Buffer.create len in
    List.iter env ~f:(fun binding ->
      Buffer.add_string block binding;
      Buffer.add_char block '\000');
    Buffer.add_char block '\000';
    Buffer.contents block
;;

let map_of_unix arr =
  Array.to_list arr
  |> List.map ~f:(fun s ->
    match String.lsplit2 s ~on:'=' with
    | None ->
      Code_error.raise
        "Env.of_unix: entry without '=' found in the environment"
        [ "var", String s ]
    | Some (k, v) -> Var.of_string k, v)
  |> Map.of_list_multi
  |> Map.map ~f:(function
    | [] -> assert false
    | x :: _ -> x)
;;

let initial = of_map (map_of_unix (Unix.environment ()))
let of_unix u = of_map (map_of_unix u)

let add t ~var ~value =
  Map.update t var ~f:(function
    | Some old when String.equal value (Value.to_string old) -> Some old
    | None | Some _ -> Some (Value.of_string value))
;;

let mem t ~var = Map.mem t var
let remove t ~var = if Map.mem t var then Map.remove t var else t
let extend t ~vars = if Map.is_empty vars then t else Map.superpose (of_map vars) t
let extend_env x y = if is_empty y then x else if is_empty x then y else Map.superpose y x
let to_dyn t = Map.to_dyn (fun value -> Dyn.string (Value.to_string value)) t

let diff x y =
  Map.merge x y ~f:(fun _ vx vy ->
    match vy with
    | Some _ -> None
    | None -> vx)
;;

let update t ~var ~f =
  match f (get t var) with
  | None -> remove t ~var
  | Some value -> add t ~var ~value
;;

let of_string_map m =
  String.Map.foldi m ~init:Map.empty ~f:(fun k v acc -> Map.set acc (Var.of_string k) v)
  |> of_map
;;

let iter t ~f = Map.iteri t ~f:(fun var value -> f var (Value.to_string value))
let to_map t = Map.map t ~f:Value.to_string
