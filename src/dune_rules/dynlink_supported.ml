module By_the_os = struct
  type t = bool

  let of_bool t = t
  let get t = t
end

type t = bool

let of_bool t = t
let get x y = x && y

let get_ocaml_config t ocaml_config =
  get t (Ocaml_config.supports_shared_libraries ocaml_config)
;;
