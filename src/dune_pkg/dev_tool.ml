open! Import

type t =
  | Ocamlformat
  | Odoc
  | Ocamllsp
  | Utop
  | Ocamlearlybird
  | Odig

let to_dyn = function
  | Ocamlformat -> Dyn.variant "Ocamlformat" []
  | Odoc -> Dyn.variant "Odoc" []
  | Ocamllsp -> Dyn.variant "Ocamllsp" []
  | Utop -> Dyn.variant "Utop" []
  | Ocamlearlybird -> Dyn.variant "Ocamlearlybird" []
  | Odig -> Dyn.variant "Odig" []
;;

let all = [ Ocamlformat; Odoc; Ocamllsp; Utop; Ocamlearlybird; Odig ]

let equal a b =
  match a, b with
  | Ocamlformat, Ocamlformat -> true
  | Ocamlformat, _ | _, Ocamlformat -> false
  | Odoc, Odoc -> true
  | Odoc, _ | _, Odoc -> false
  | Ocamllsp, Ocamllsp -> true
  | Ocamllsp, _ | _, Ocamllsp -> false
  | Utop, Utop -> true
  | Utop, _ | _, Utop -> false
  | Ocamlearlybird, Ocamlearlybird -> true
  | Ocamlearlybird, _ | _, Ocamlearlybird -> false
  | Odig, Odig -> true
;;

let hash = Poly.hash

let package_name = function
  | Ocamlformat -> Package_name.of_string "ocamlformat"
  | Odoc -> Package_name.of_string "odoc"
  | Ocamllsp -> Package_name.of_string "ocaml-lsp-server"
  | Utop -> Package_name.of_string "utop"
  | Ocamlearlybird -> Package_name.of_string "earlybird"
  | Odig -> Package_name.of_string "odig"
;;

let of_package_name package_name =
  match Package_name.to_string package_name with
  | "ocamlformat" -> Ocamlformat
  | "odoc" -> Odoc
  | "ocaml-lsp-server" -> Ocamllsp
  | "utop" -> Utop
  | "earlybird" -> Ocamlearlybird
  | "odig" -> Odig
  | other -> User_error.raise [ Pp.textf "No such dev tool: %s" other ]
;;

let exe_name = function
  | Ocamlformat -> "ocamlformat"
  | Odoc -> "odoc"
  | Ocamllsp -> "ocamllsp"
  | Utop -> "utop"
  | Ocamlearlybird -> "ocamlearlybird"
  | Odig -> "odig"
;;

let exe_path_components_within_package t =
  match t with
  | Ocamlformat -> [ "bin"; exe_name t ]
  | Odoc -> [ "bin"; exe_name t ]
  | Ocamllsp -> [ "bin"; exe_name t ]
  | Utop -> [ "bin"; exe_name t ]
  | Ocamlearlybird -> [ "bin"; exe_name t ]
  | Odig -> [ "bin"; exe_name t ]
;;

let needs_to_build_with_same_compiler_as_project = function
  | Ocamlformat -> false
  | Odoc -> true
  | Ocamllsp -> true
  | Utop -> false
  | Ocamlearlybird -> false
  | Odig -> false
;;
