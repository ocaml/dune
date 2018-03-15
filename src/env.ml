open Import

module Var = struct
  type t = string
  let compare =
    if Sys.win32 then (
      fun a b -> String.compare (String.lowercase a) (String.lowercase b)
    ) else (
      String.compare
    )
end

module Map = Map.Make(Var)

type t =
  { vars : string Map.t
  ; mutable unix : string array option
  }

let make vars =
  { vars
  ; unix = None
  }

let get t k = Map.find t.vars k

let to_unix t =
  match t.unix with
  | Some v -> v
  | None ->
    let res =
      Map.foldi ~init:[] ~f:(fun k v acc ->
        (sprintf "%s=%s" k v)::acc
      ) t.vars
      |> Array.of_list in
    t.unix <- Some res;
    res

let of_unix arr =
  Array.to_list arr
  |> List.map ~f:(fun s ->
    match String.lsplit2 s ~on:'=' with
    | None ->
      Sexp.code_error "Env.of_unix: entry without '=' found in the environ"
        ["var", Sexp.To_sexp.string s]
    | Some (k, v) -> (k, v))
  |> Map.of_list_multi
  |> Map.map ~f:(function
    | [] -> assert false
    | x::_ -> x)

let initial =
  let i =
    lazy (
      make (Lazy.force Colors.setup_env_for_colors;
            Unix.environment ()
            |> of_unix)
    ) in
  fun () -> Lazy.force i

let add t ~var ~value =
  make (Map.add t.vars var value)

let extend t ~vars =
  make (Map.union t.vars vars ~f:(fun _ _ v -> Some v))

let extend_env x y =
  extend x ~vars:y.vars

let sexp_of_t t =
  let open Sexp.To_sexp in
  (list (pair string string)) (Map.to_list t.vars)

let diff x y =
  Map.merge x.vars y.vars ~f:(fun _k vx vy ->
    match vy with
    | Some _ -> None
    | None -> vx)
  |> make
