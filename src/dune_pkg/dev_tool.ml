open! Import

type t =
  | Ocamlformat
  | Odoc
  | Ocamllsp
  | Utop
  | Ocamlearlybird
  | Odig
  | Opam_publish
  | Dune_release

let to_dyn = function
  | Ocamlformat -> Dyn.variant "Ocamlformat" []
  | Odoc -> Dyn.variant "Odoc" []
  | Ocamllsp -> Dyn.variant "Ocamllsp" []
  | Utop -> Dyn.variant "Utop" []
  | Ocamlearlybird -> Dyn.variant "Ocamlearlybird" []
  | Odig -> Dyn.variant "Odig" []
  | Opam_publish -> Dyn.variant "Opam_publish" []
  | Dune_release -> Dyn.variant "Dune_release" []
;;

let all =
  [ Ocamlformat; Odoc; Ocamllsp; Utop; Ocamlearlybird; Odig; Opam_publish; Dune_release ]
;;

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
  | Odig, _ | _, Odig -> false
  | Opam_publish, Opam_publish -> true
  | Opam_publish, _ -> false
  | _, Opam_publish -> false
  | Dune_release, Dune_release -> true
;;

let hash = Poly.hash

let to_string = function
  | Ocamlformat -> "ocamlformat"
  | Odoc -> "odoc"
  | Ocamllsp -> "ocaml-lsp-server"
  | Utop -> "utop"
  | Ocamlearlybird -> "earlybird"
  | Odig -> "odig"
  | Opam_publish -> "opam-publish"
  | Dune_release -> "dune-release"
;;

let package_name v = v |> to_string |> Package_name.of_string

let of_string = function
  | "ocamlformat" -> Some Ocamlformat
  | "odoc" -> Some Odoc
  | "ocaml-lsp-server" -> Some Ocamllsp
  | "utop" -> Some Utop
  | "earlybird" -> Some Ocamlearlybird
  | "odig" -> Some Odig
  | "opam-publish" -> Some Opam_publish
  | "dune-release" -> Some Dune_release
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
  | Opam_publish -> "opam-publish"
  | Dune_release -> "dune-release"
;;

let exe_path_components_within_package t = [ "bin"; exe_name t ]

let needs_to_build_with_same_compiler_as_project = function
  | Ocamlformat -> false
  | Ocamlearlybird ->
    (* CR-someday rgrinberg: I have my doubts that this is true given that
       the debugger is expected to print identifiers. In any case,
       ocamlearlybird isn't going to work well due to relocation issues *)
    false
  | Opam_publish -> false
  | Dune_release -> false
  | Utop | Odoc | Ocamllsp | Odig -> true
;;
