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

let to_string = function
  | Ocamlformat -> "ocamlformat"
  | Odoc -> "odoc"
  | Ocamllsp -> "ocaml-lsp-server"
  | Utop -> "utop"
  | Ocamlearlybird -> "earlybird"
  | Odig -> "odig"
;;

let package_name v = v |> to_string |> Package_name.of_string

let of_string = function
  | "ocamlformat" -> Some Ocamlformat
  | "odoc" -> Some Odoc
  | "ocaml-lsp-server" -> Some Ocamllsp
  | "utop" -> Some Utop
  | "earlybird" -> Some Ocamlearlybird
  | "odig" -> Some Odig
  | _otherwise -> None
;;

let of_package_name package_name =
  let s = Package_name.to_string package_name in
  match of_string s with
  | Some dev_tool -> dev_tool
  | None -> User_error.raise [ Pp.textf "No such dev tool: %s" s ]
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
