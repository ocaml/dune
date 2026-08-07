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

  let temp_dir = if Sys.win32 then "TEMP" else "TMPDIR"

  include Comparable.Make (T)
  include T
end

module Set = Var.Set
module Map = Var.Map

module Binding = struct
  type t =
    { value : string
    ; mutable rendered : string Option.Unboxed.t
    }

  let create value = { value; rendered = Option.Unboxed.none }

  let render t ~var =
    if Option.Unboxed.is_some t.rendered
    then Option.Unboxed.value_exn t.rendered
    else (
      let rendered = String.append_with_char var ~sep:'=' t.value in
      t.rendered <- Option.Unboxed.some rendered;
      rendered)
  ;;
end

(* The use of [mutable] for these caches is safe, since we never call (back) to
   the memoization framework when computing [unix]. *)
type t =
  { vars : Binding.t Map.t
  ; mutable unix : string list option
  }

let equal t { vars; unix = _ } =
  Map.equal
    ~equal:(fun { Binding.value = x; _ } { Binding.value = y; _ } -> String.equal x y)
    t.vars
    vars
;;

let hash { vars; unix = _ } =
  Map.foldi vars ~init:(Hash.create ()) ~f:(fun var { Binding.value; _ } acc ->
    let acc = Hash.feed acc (Var.hash var) in
    Hash.feed acc (String.hash value))
  |> Hash.hash
;;

let of_bindings vars = { vars; unix = None }
let of_map vars = Map.map vars ~f:Binding.create |> of_bindings
let empty = of_bindings Map.empty
let is_empty t = Map.is_empty t.vars
let vars t = Var.Set.of_keys t.vars
let get t k = Option.map (Map.find t.vars k) ~f:(fun { Binding.value; _ } -> value)

let to_unix t =
  match t.unix with
  | Some v -> v
  | None ->
    let res =
      Map.foldi t.vars ~init:[] ~f:(fun var binding acc ->
        Binding.render binding ~var :: acc)
    in
    t.unix <- Some res;
    res
;;

let map_of_unix arr =
  Array.to_list arr
  |> List.map ~f:(fun s ->
    match String.lsplit2 s ~on:'=' with
    | None ->
      Code_error.raise
        "Env.of_unix: entry without '=' found in the environment"
        [ "var", String s ]
    | Some (k, v) -> k, v)
  |> Map.of_list_multi
  |> Map.map ~f:(function
    | [] -> assert false
    | x :: _ -> x)
;;

let initial = of_map (map_of_unix (Unix.environment ()))
let of_unix u = of_map (map_of_unix u)
let add t ~var ~value = of_bindings (Map.set t.vars var (Binding.create value))
let mem t ~var = Map.mem t.vars var
let remove t ~var = of_bindings (Map.remove t.vars var)

let extend t ~vars =
  if Map.is_empty vars
  then t
  else of_bindings (Map.superpose (Map.map vars ~f:Binding.create) t.vars)
;;

let extend_env x y =
  if is_empty y
  then x
  else if is_empty x
  then y
  else of_bindings (Map.superpose y.vars x.vars)
;;

let to_dyn t =
  let open Dyn in
  Map.to_dyn (fun { Binding.value; _ } -> string value) t.vars
;;

let diff x y =
  Map.merge x.vars y.vars ~f:(fun _k vx vy ->
    match vy with
    | Some _ -> None
    | None -> vx)
  |> of_bindings
;;

let update t ~var ~f =
  let old = Option.map (Map.find t.vars var) ~f:(fun { Binding.value; _ } -> value) in
  match f old with
  | None -> remove t ~var
  | Some value -> add t ~var ~value
;;

let of_string_map m =
  of_map (String.Map.foldi ~init:Map.empty ~f:(fun k v acc -> Map.set acc k v) m)
;;

let iter t ~f = Map.iteri t.vars ~f:(fun var { Binding.value; _ } -> f var value)
let to_map t = Map.map t.vars ~f:(fun { Binding.value; _ } -> value)
