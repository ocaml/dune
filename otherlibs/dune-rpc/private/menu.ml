open Import
open Types

(* This list of methods and generations is exactly the list of existing methods
   as of commit [3cd240e], which the initial versioning implementation was based
   on. *)
let compatibility_menu =
  Method.Name.Map.of_list_exn
    [ ("ping", 1)
    ; ("diagnostics", 1)
    ; ("shutdown", 1)
    ; ("subscribe", 1)
    ; ("unsubscribe", 1)
    ; ("build", 1)
    ; ("status", 1)
    ; ("notify/abort", 1)
    ; ("notify/diagnostic", 1)
    ; ("notify/log", 1)
    ; ("notify/progress", 1)
    ]

type t = Method.Version.t Method.Name.Map.t

let default = compatibility_menu

let find = Method.Name.Map.find

let select_common ~local_versions ~remote_versions =
  let selected_versions =
    List.filter_map remote_versions ~f:(fun (method_, remote_versions) ->
        let remote_versions = Method.Version.Set.of_list remote_versions in
        let open Option.O in
        let* local_versions = Method.Name.Map.find local_versions method_ in
        let+ greatest_common_version =
          Method.Version.Set.max_elt
            (Method.Version.Set.inter remote_versions local_versions)
        in
        (method_, greatest_common_version))
  in
  match selected_versions with
  | [] -> None
  | _ :: _ -> Some (Method.Name.Map.of_list_exn selected_versions)

let of_list = Method.Name.Map.of_list

let to_list = Method.Name.Map.to_list

let to_dyn = Method.Name.Map.to_dyn Int.to_dyn
