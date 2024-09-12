open! Import

type t = Ocamlformat

let equal a b =
  match a, b with
  | Ocamlformat, Ocamlformat -> true
;;

let package_name = function
  | Ocamlformat -> Package_name.of_string "ocamlformat"
;;

let of_package_name package_name =
  match Package_name.to_string package_name with
  | "ocamlformat" -> Ocamlformat
  | other -> User_error.raise [ Pp.textf "No such dev tool: %s" other ]
;;

let exe_name = function
  | Ocamlformat -> "ocamlformat"
;;

let exe_path_components_within_package t =
  match t with
  | Ocamlformat -> [ "bin"; exe_name t ]
;;
