open Stdune

let _BUILD_PATH_PREFIX_MAP = "BUILD_PATH_PREFIX_MAP"

let extend_build_path_prefix_map env how map =
  let new_rules = Build_path_prefix_map.encode_map map in
  Env.update env ~var:_BUILD_PATH_PREFIX_MAP ~f:(function
    | None -> Some new_rules
    | Some existing_rules -> (
      (* Check the validity of the existing rules. *)
      match Build_path_prefix_map.decode_map existing_rules with
      | Error err ->
        Printf.printf "extend_build_path_prefix_map, err=%s\n%!" err;

        (* Existing is invalid, so just use new. *)
        Some new_rules
      | Ok _ ->
        (* Existing are ok, combine. *)
        Some
          (match how with
          | `Existing_rules_have_precedence -> new_rules ^ ":" ^ existing_rules
          | `New_rules_have_precedence -> existing_rules ^ ":" ^ new_rules)))

(* Mappingd str needed in two contexts, when building, and
   when deploying (e.g. debugger using a built bytecode file). *)
let map_for_build_context =
  ref (String.Map.empty : Build_path_prefix_map.map String.Map.t)

let extend_build_map_for_context env how root =
  let open Printf in
  let debug = false in
  if debug then printf "\nextend_build_map_for_context: root=%s\n%!" root;
  let module Bppm = Build_path_prefix_map in
  let context_name = Filename.basename root in
  match String.Map.find !map_for_build_context context_name with
  | None -> env (* Should this be an error? *)
  | Some map ->
    if debug then
      List.iter map ~f:(fun opt_item ->
          match opt_item with
          | None -> ()
          | Some { Bppm.source; target } ->
            printf "Before: %s -> %s\n%!" source target);
    (* The mappings in this map that are relative, are relative
       to root, so adjust them. *)
    let map =
      List.map map ~f:(fun opt_item ->
          Option.map opt_item ~f:(fun { Bppm.source; target } ->
              if Filename.is_relative source then
                { Bppm.source =
                    (if source = "" then root else Filename.concat root source)
                ; target
                }
              else { Bppm.source; target }))
    in
    if debug then
      List.iter map ~f:(fun opt_item ->
          match opt_item with
          | None -> ()
          | Some { Bppm.source; target } ->
            printf "After: %s -> %s\n%!" source target);
    extend_build_path_prefix_map env how map

let _DEPLOY_PATH_PREFIX_MAP = "DEPLOY_PATH_PREFIX_MAP"

let extend_deploy_path_prefix_map env how map =
  let new_rules = Build_path_prefix_map.encode_map map in
  Env.update env ~var:_DEPLOY_PATH_PREFIX_MAP ~f:(function
    | None -> Some new_rules
    | Some existing_rules ->
      Some
        (match how with
        | `Existing_rules_have_precedence -> new_rules ^ ":" ^ existing_rules
        | `New_rules_have_precedence -> existing_rules ^ ":" ^ new_rules))

let map_for_deploy_context =
  ref (String.Map.empty : Build_path_prefix_map.map String.Map.t)

(* For the deployment map, we do not want to use any existing
   value for the mapping variable. *)
let extend_deploy_map_for_context env how context_name =
  match String.Map.find !map_for_deploy_context context_name with
  | None -> env (* Should this be an error? *)
  | Some map -> extend_deploy_path_prefix_map env how map
