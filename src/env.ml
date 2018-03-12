open Import

module Var = struct
  type t = string
  let compare a b =
    if Sys.win32 then
      String.compare (String.lowercase a) (String.lowercase b)
    else
      String.compare a b

end

module Map = Map.Make(Var)

type t =
  { base : string array
  ; extra : string Map.t
  ; mutable unix : string array option
  }

let make ~base ~extra =
  { base
  ; extra
  ; unix = None
  }

let get_env_base env var =
  let rec loop i =
    if i = Array.length env then
      None
    else
      let entry = env.(i) in
      match String.lsplit2 entry ~on:'=' with
      | Some (key, value) when Var.compare key var = Eq ->
        Some value
      | _ -> loop (i + 1)
  in
  loop 0

let get t v =
  match Map.find t.extra v with
  | None -> get_env_base t.base v
  | Some _ as v -> v

let to_unix t =
  match t.unix with
  | Some v -> v
  | None ->
    let res =
      if Map.is_empty t.extra then
        t.base
      else
        let imported =
          Array.to_list t.base
          |> List.filter ~f:(fun s ->
            match String.index s '=' with
            | None -> true
            | Some i ->
              let key = String.sub s ~pos:0 ~len:i in
              not (Map.mem t.extra key))
        in
        List.rev_append
          (List.map (Map.to_list t.extra)
             ~f:(fun (k, v) -> sprintf "%s=%s" k v))
          imported
        |> Array.of_list in
    t.unix <- Some res;
    res

let initial =
  let i =
    lazy (
      make
        ~base:(Lazy.force Colors.setup_env_for_colors;
               Unix.environment ())
        ~extra:Map.empty
    ) in
  fun () -> Lazy.force i

let extend t ~vars =
  make ~base:t.base
    ~extra:(
      Map.merge t.extra vars ~f:(fun _ v1 v2 ->
        match v2 with
        | Some _ -> v2
        | None -> v1)
    )

let add t ~var ~value =
  make ~base:t.base ~extra:(Map.add t.extra var value)
