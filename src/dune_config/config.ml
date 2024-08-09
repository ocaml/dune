open Stdune

let initialized = ref false
let configure_time_frozen = ref false

type 'a t =
  { name : string
  ; of_string : string -> ('a, string) result
  ; mutable value : 'a option
  ; default : 'a
  ; mutable configure_time_value : 'a option
  }

let env_name t = sprintf "DUNE_CONFIG__%s" (String.uppercase_ascii t.name)

let get t =
  if not !initialized
  then Code_error.raise "Config.get: invalid access" [ "name", Dyn.string t.name ];
  configure_time_frozen := true;
  match t.value with
  | Some v -> v
  | None -> Option.value t.configure_time_value ~default:t.default
;;

type packed = E : 'a t -> packed

let all = ref []
let register t = all := E t :: !all
let toggles = ref []
let register_toggle t = toggles := t :: !toggles

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
    | None -> Option.iter config ~f:(fun config -> t.value <- Some config)
    | Some v ->
      (match t.of_string v with
       | Ok v -> t.value <- Some v
       | Error e ->
         User_error.raise [ Pp.textf "Invalid value for %S" env_name; Pp.text e ]));
  initialized := true
;;

let make ~name ~of_string ~default =
  let t = { name; of_string; value = None; default; configure_time_value = None } in
  register t;
  t
;;

let set_configure_time_toggles ~names =
  if !configure_time_frozen
  then Code_error.raise "Config.set_configure_time_toggles: invalid access" [];
  List.iter names ~f:(fun name ->
    let t = List.find_exn !toggles ~f:(fun t -> String.equal t.name name) in
    t.configure_time_value <- Some `Enabled);
  configure_time_frozen := true
;;

let make_toggle ~name ~default =
  let t = make ~name ~default ~of_string:Toggle.of_string in
  register_toggle t;
  t
;;

let global_lock = make_toggle ~name:"global_lock" ~default:`Enabled

let cutoffs_that_reduce_concurrency_in_watch_mode =
  make_toggle ~name:"cutoffs_that_reduce_concurrency_in_watch_mode" ~default:`Disabled
;;

let copy_file =
  make
    ~name:"copy_file"
    ~of_string:(function
      | "portable" -> Ok `Portable
      | "fast" -> Ok `Best
      | _ -> Error (sprintf "only %S and %S are allowed" "fast" "portable"))
    ~default:`Best
;;

let background_default =
  match Platform.OS.value with
  | Linux | Windows | Darwin -> `Enabled
  | _ -> `Disabled
;;

let background_actions = make_toggle ~name:"background_actions" ~default:`Disabled

let background_digests =
  make_toggle ~name:"background_digests" ~default:background_default
;;

let background_sandboxes =
  make_toggle ~name:"background_sandboxes" ~default:background_default
;;

let background_file_system_operations_in_rule_execution =
  make_toggle
    ~name:"background_file_system_operations_in_rule_execution"
    ~default:`Disabled
;;

let threaded_console = make_toggle ~name:"threaded_console" ~default:background_default

let threaded_console_frames_per_second =
  make
    ~name:"threaded_console_frames_per_second"
    ~of_string:(fun x ->
      match Int.of_string x with
      | Some x when x > 0 && x <= 1000 -> Ok (`Custom x)
      | Some _ -> Error (sprintf "value must be between 1 and 1000")
      | None -> Error (sprintf "could not parse %S as an integer" x))
    ~default:`Default
;;

let party_mode = make_toggle ~name:"party_mode" ~default:`Disabled
