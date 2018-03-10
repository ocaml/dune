open Import

let initial_env = lazy (
  Lazy.force Colors.setup_env_for_colors;
  Unix.environment ())

module Var = struct
  type t = string
  let compare a b =
    if Sys.win32 then
      String.compare (String.lowercase a) (String.lowercase b)
    else
      String.compare a b

end

let get_env env var =
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

module Map = Map.Make(Var)

let extend_env ~vars ~env =
  if Map.is_empty vars then
    env
  else
    let imported =
      Array.to_list env
      |> List.filter ~f:(fun s ->
        match String.index s '=' with
        | None -> true
        | Some i ->
          let key = String.sub s ~pos:0 ~len:i in
          not (Map.mem vars key))
    in
    List.rev_append
      (List.map (Map.to_list vars)
         ~f:(fun (k, v) -> sprintf "%s=%s" k v))
      imported
    |> Array.of_list


