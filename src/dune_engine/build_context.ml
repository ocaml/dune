open Import

type t =
  { name : Context_name.t
  ; build_dir : Path.Build.t
  }

let create ~name =
  let build_dir = Path.Build.of_string (Context_name.to_string name) in
  { name; build_dir }
;;

let of_build_path p =
  match Dpath.Target_dir.of_target p with
  | Regular (With_context (name, _)) | Anonymous_action (With_context (name, _)) ->
    Some (create ~name)
  | Regular Root | Anonymous_action Root | Invalid _ -> None
;;
