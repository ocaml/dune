open Import

type t =
  | Ocamlformat
  | Odoc
  | Ocamllsp
  | Utop
  | Ocamlearlybird
  | Odig
  | Opam_publish
  | Dune_release
  | Ocaml_index
  | Merlin

let to_dyn = function
  | Ocamlformat -> Dyn.variant "Ocamlformat" []
  | Odoc -> Dyn.variant "Odoc" []
  | Ocamllsp -> Dyn.variant "Ocamllsp" []
  | Utop -> Dyn.variant "Utop" []
  | Ocamlearlybird -> Dyn.variant "Ocamlearlybird" []
  | Odig -> Dyn.variant "Odig" []
  | Opam_publish -> Dyn.variant "Opam_publish" []
  | Dune_release -> Dyn.variant "Dune_release" []
  | Ocaml_index -> Dyn.variant "Ocaml_index" []
  | Merlin -> Dyn.variant "Merlin" []
;;

let all =
  [ Ocamlformat
  ; Odoc
  ; Ocamllsp
  ; Utop
  ; Ocamlearlybird
  ; Odig
  ; Opam_publish
  ; Dune_release
  ; Ocaml_index
  ; Merlin
  ]
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
  | Dune_release, _ -> false
  | _, Dune_release -> false
  | Ocaml_index, Ocaml_index -> true
  | Ocaml_index, _ | _, Ocaml_index -> false
  | Merlin, Merlin -> true
;;

let hash = Poly.hash

let package_name = function
  | Ocamlformat -> Package_name.of_string "ocamlformat"
  | Odoc -> Package_name.of_string "odoc"
  | Ocamllsp -> Package_name.of_string "ocaml-lsp-server"
  | Utop -> Package_name.of_string "utop"
  | Ocamlearlybird -> Package_name.of_string "earlybird"
  | Odig -> Package_name.of_string "odig"
  | Opam_publish -> Package_name.of_string "opam-publish"
  | Dune_release -> Package_name.of_string "dune-release"
  | Ocaml_index -> Package_name.of_string "ocaml-index"
  | Merlin -> Package_name.of_string "merlin"
;;

let of_package_name package_name =
  match Package_name.to_string package_name with
  | "ocamlformat" -> Ocamlformat
  | "odoc" -> Odoc
  | "ocaml-lsp-server" -> Ocamllsp
  | "utop" -> Utop
  | "earlybird" -> Ocamlearlybird
  | "odig" -> Odig
  | "opam-publish" -> Opam_publish
  | "dune-release" -> Dune_release
  | "ocaml-index" -> Ocaml_index
  | "merlin" -> Merlin
  | other -> User_error.raise [ Pp.textf "No such dev tool: %s" other ]
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
  | Ocaml_index -> "ocaml-index"
  | Merlin -> "ocamlmerlin"
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
  | Utop | Odoc | Ocamllsp | Ocaml_index | Odig | Merlin -> true
;;

let compiler_package_names =
  (* TODO don't hardcode these names here *)
  [ Package_name.of_string "ocaml-base-compiler"
  ; Package_name.of_string "ocaml-variants"
  ; Package_name.of_string "ocaml-compiler"
    (* The [ocaml-compiler] package is required to include all the
           packages that might install a compiler, starting from ocaml.5.3.0.
    *)
  ]
;;
