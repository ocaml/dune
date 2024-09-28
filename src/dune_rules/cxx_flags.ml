open Import

type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

let base_cxx_compile_flags ocaml_config = function
  | Gcc | Clang ->
    "-x"
    :: "c++"
    :: (if Ocaml_config.version ocaml_config >= (5, 0, 0) then [ "-std=c++11" ] else [])
  | Msvc -> [ "/TP" ]
  | Other _ -> []
;;

let base_cxx_link_flags = function
  | Gcc -> [ "-lstdc++"; "-shared-libgcc" ]
  | Clang -> [ "-lc++" ]
  | Msvc -> []
  | Other _ -> []
;;

let fdiagnostics_color = function
  | Gcc | Clang -> [ "-fdiagnostics-color=always" ]
  | _ -> []
;;

let preprocessed_filename = "ccomp"

let ccomp_type build_dir =
  let open Action_builder.O in
  let filepath =
    Path.Build.(relative (relative build_dir ".dune/ccomp") preprocessed_filename)
  in
  let+ ccomp = Action_builder.contents (Path.build filepath) in
  match String.trim ccomp with
  | "clang" -> Clang
  | "gcc" -> Gcc
  | "msvc" -> Msvc
  | s -> Other s
;;

let check_warn = function
  | Other s ->
    User_warning.emit
      [ Pp.textf
          "Dune was not able to automatically infer the C compiler in use: \"%s\". \
           Please open an issue on github to help us improve this feature."
          s
      ]
  | _ -> ()
;;

let ccomp_type (ctx : Build_context.t) =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx.build_dir in
  check_warn ccomp_type;
  ccomp_type
;;

let get_compile_flags ocaml_version ctx =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx in
  base_cxx_compile_flags ocaml_version ccomp_type
;;

let get_link_flags ctx =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx in
  base_cxx_link_flags ccomp_type
;;
