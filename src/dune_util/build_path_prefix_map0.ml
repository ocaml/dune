open Stdune

let _BUILD_PATH_PREFIX_MAP = "BUILD_PATH_PREFIX_MAP"

let extend_build_path_prefix_map env how map =
  Env.update env ~var:_BUILD_PATH_PREFIX_MAP ~f:(fun old_var_val ->
      let new_map =
        match old_var_val with
        | None -> map
        | Some existing_rules -> (
          (* Validate the existing rules. *)
          match Build_path_prefix_map.decode_map existing_rules with
          | Error _ ->
            (* Ignore invalid existing maps. *)
            map
          | Ok existing_map -> (
            match how with
            | `Existing_rules_have_precedence -> map @ existing_map
            | `New_rules_have_precedence -> existing_map @ map))
      in
      Some (Build_path_prefix_map.encode_map new_map))
