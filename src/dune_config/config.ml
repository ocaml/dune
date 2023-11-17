open Stdune

let initialized = ref false

type 'a t =
  { name : string
  ; of_string : string -> ('a, string) result
  ; mutable value : 'a
  }

let env_name t = sprintf "DUNE_CONFIG__%s" (String.uppercase_ascii t.name)

let get t =
  if not !initialized
  then Code_error.raise "Config.get: invalid access" [ "name", Dyn.string t.name ];
  t.value
;;

type packed = E : 'a t -> packed

let all = ref []
let register t = all := E t :: !all

module Toggle = struct
  type t =
    [ `Enabled
    | `Disabled
    ]

  let all : (string * t) list = [ "enabled", `Enabled; "disabled", `Disabled ]

  let to_string t =
    List.find_map all ~f:(fun (k, v) -> if Poly.equal v t then Some k else None)
    |> Option.value_exn
  ;;

  let of_string s =
    match List.assoc all s with
    | Some s -> Ok s
    | None -> Error (sprintf "only %S and %S are allowed" "enabled" "disabled")
  ;;

  let to_dyn =
    let open Dyn in
    function
    | `Enabled -> variant "Enabled" []
    | `Disabled -> variant "Disabled" []
  ;;
end

let init values =
  if !initialized then Code_error.raise "Config.init: already initialized" [];
  let all =
    let all = String.Map.of_list_map_exn !all ~f:(fun (E t) -> t.name, E t) in
    String.Map.merge values all ~f:(fun name x y ->
      match x, y with
      | None, None -> assert false
      | Some (loc, _), None ->
        User_error.raise ~loc [ Pp.textf "key %S doesn't exist" name ]
      | Some (loc, v), Some t -> Some (Some (loc, v), t)
      | None, Some t -> Some (None, t))
  in
  String.Map.iter all ~f:(fun (config, E t) ->
    let config =
      Option.map config ~f:(fun (loc, config) ->
        match t.of_string config with
        | Ok s -> s
        | Error v ->
          User_error.raise ~loc [ Pp.textf "failed to parse %S" t.name; Pp.text v ])
    in
    let env_name = env_name t in
    match Sys.getenv_opt env_name with
    | None -> Option.iter config ~f:(fun config -> t.value <- config)
    | Some v ->
      (match t.of_string v with
       | Ok v -> t.value <- v
       | Error e ->
         User_error.raise [ Pp.textf "Invalid value for %S" env_name; Pp.text e ]));
  initialized := true
;;

let global_lock =
  let t = { name = "global_lock"; of_string = Toggle.of_string; value = `Enabled } in
  register t;
  t
;;

let cutoffs_that_reduce_concurrency_in_watch_mode =
  let t =
    { name = "cutoffs_that_reduce_concurrency_in_watch_mode"
    ; of_string = Toggle.of_string
    ; value = `Disabled
    }
  in
  register t;
  t
;;

let copy_file =
  let t =
    { name = "copy_file"
    ; of_string =
        (function
          | "portable" -> Ok `Portable
          | "fast" -> Ok `Best
          | _ -> Error (sprintf "only %S and %S are allowed" "fast" "portable"))
    ; value = `Best
    }
  in
  register t;
  t
;;

let background_default =
  match Platform.OS.value with
  | Linux | Windows | Darwin -> `Enabled
  | _ -> `Disabled
;;

let background_actions =
  let t =
    { name = "background_actions"; of_string = Toggle.of_string; value = `Disabled }
  in
  register t;
  t
;;

let background_digests =
  let t =
    { name = "background_digests"
    ; of_string = Toggle.of_string
    ; value = background_default
    }
  in
  register t;
  t
;;

let background_sandboxes =
  let t =
    { name = "background_sandboxes"
    ; of_string = Toggle.of_string
    ; value = background_default
    }
  in
  register t;
  t
;;

let background_dune_rules =
  let t =
    { name = "background_dune_rules"; of_string = Toggle.of_string; value = `Disabled }
  in
  register t;
  t
;;

let background_file_system_operations_in_rule_execution =
  let t =
    { name = "background_file_system_operations_in_rule_execution"
    ; of_string = Toggle.of_string
    ; value = `Disabled
    }
  in
  register t;
  t
;;

let threaded_console =
  let t =
    { name = "threaded_console"
    ; of_string = Toggle.of_string
    ; value = background_default
    }
  in
  register t;
  t
;;

let threaded_console_frames_per_second =
  let t =
    { name = "threaded_console_frames_per_second"
    ; of_string =
        (fun x ->
          match Int.of_string x with
          | Some x when x > 0 && x <= 1000 -> Ok (`Custom x)
          | Some _ -> Error (sprintf "value must be between 1 and 1000")
          | None -> Error (sprintf "could not parse %S as an integer" x))
    ; value = `Default
    }
  in
  register t;
  t
;;
