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
  match Dpath.analyse_target p with
  | Regular (name, _) | Alias (name, _) | Anonymous_action name -> Some (create ~name)
  | Other _ -> None
;;
