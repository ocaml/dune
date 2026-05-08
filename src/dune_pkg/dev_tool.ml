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

let repr =
  Repr.variant
    "dev-tool"
    [ Repr.case0 "Ocamlformat" ~test:(function
        | Ocamlformat -> true
        | _ -> false)
    ; Repr.case0 "Odoc" ~test:(function
        | Odoc -> true
        | _ -> false)
    ; Repr.case0 "Ocamllsp" ~test:(function
        | Ocamllsp -> true
        | _ -> false)
    ; Repr.case0 "Utop" ~test:(function
        | Utop -> true
        | _ -> false)
    ; Repr.case0 "Ocamlearlybird" ~test:(function
        | Ocamlearlybird -> true
        | _ -> false)
    ; Repr.case0 "Odig" ~test:(function
        | Odig -> true
        | _ -> false)
    ; Repr.case0 "Opam_publish" ~test:(function
        | Opam_publish -> true
        | _ -> false)
    ; Repr.case0 "Dune_release" ~test:(function
        | Dune_release -> true
        | _ -> false)
    ; Repr.case0 "Ocaml_index" ~test:(function
        | Ocaml_index -> true
        | _ -> false)
    ; Repr.case0 "Merlin" ~test:(function
        | Merlin -> true
        | _ -> false)
    ]
;;

let to_dyn = Repr.to_dyn repr

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

let equal, _ = Repr.make_compare repr
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

(* Package names that are considered compiler packages. When these are pinned
   in the project, the pins should be propagated to dev tools that need to be
   built with the same compiler. This list is also used by pkg_toolchain. *)
let compiler_package_names =
  List.map
    ~f:Package_name.of_string
    [ "ocaml"
    ; "ocaml-base-compiler"
    ; "ocaml-variants"
    ; "ocaml-compiler"
    ; "relocatable-compiler"
    ]
;;

let is_compiler_package name =
  List.mem compiler_package_names name ~equal:Package_name.equal
;;
