open Stdune

let _BUILD_PATH_PREFIX_MAP = "BUILD_PATH_PREFIX_MAP"

let extend_build_path_prefix_map env how map =
  let new_rules = Build_path_prefix_map.encode_map map in
  Env.update env ~var:_BUILD_PATH_PREFIX_MAP ~f:(function
    | None -> Some new_rules
    | Some existing_rules ->
      Some
        (match how with
         | `Existing_rules_have_precedence -> new_rules ^ ":" ^ existing_rules
         | `New_rules_have_precedence -> existing_rules ^ ":" ^ new_rules))
;;
