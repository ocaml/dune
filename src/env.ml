open Import

module Var = struct
  type t = string
  let compare a b =
    if Sys.win32 then
      String.compare (String.lowercase a) (String.lowercase b)
    else
      String.compare a b

  let equal a b =
    match compare a b with
    | Ordering.Eq -> true
    | _ -> false

  let hash =
    if Sys.win32 then
      fun x -> Hashtbl.hash (String.lowercase x)
    else
      Hashtbl.hash
end

module Map = Map.Make(Var)

module Table = Hashtbl.Make(Var)

type t =
  { vars : (Var.t * string) list
  ; mutable unix : string array option
  }

let make vars =
  { vars
  ; unix = None
  }

let get t k =
  List.find_map t.vars ~f:(fun (k', v) ->
    match Var.compare k k' with
    | Ordering.Eq -> Some v
    | _ -> None)

let to_unix t =
  match t.unix with
  | Some v -> v
  | None ->
    let res =
      let seen = Table.create 16 in
      t.vars
      |> List.fold_left ~init:[] ~f:(fun uniques (k, v) ->
        if Table.mem seen k then (
          uniques
        ) else (
          Table.add seen ~key:k ~data:();
          (k, v) :: uniques
        ))
      |> List.rev_map ~f:(fun (k, v) -> sprintf "%s=%s" k v)
      |> Array.of_list in
    t.unix <- Some res;
    res

let of_unix arr =
  Array.to_list arr
  |> List.map ~f:(fun s ->
    match String.lsplit2 s ~on:'=' with
    | None -> (s, "")
    | Some (k, v) -> (k, v))

let initial =
  let i =
    lazy (
      make (Lazy.force Colors.setup_env_for_colors;
            Unix.environment ()
            |> of_unix)
    ) in
  fun () -> Lazy.force i

let add t ~var ~value =
  { vars = (var, value) :: t.vars
  ; unix = None
  }

let extend t ~vars =
  { vars = Map.foldi ~init:t.vars ~f:(fun k v t -> (k, v) :: t) vars
  ; unix = None
  }

let sexp_of_t t =
  let open Sexp.To_sexp in
  (list (pair string string)) t.vars

let diff x y =
  let to_map b = Map.of_list_reduce b ~f:(fun old _new -> old) in
  Map.merge (to_map x.vars) (to_map y.vars) ~f:(fun _k vx vy ->
    match vy with
    | Some _ -> None
    | None -> vx)
  |> Map.to_list
  |> make
