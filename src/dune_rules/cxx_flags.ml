open Import

type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

type phase =
  | Compile of Ocaml.Version.t
  | Link

let base_cxx_compile_flags version = function
  | Gcc | Clang ->
    "-x"
    :: "c++"
    :: (if Ocaml.Version.add_std_cxx_flag version then [ "-std=gnu++11" ] else [])
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
  | (Gcc | Clang) when Lazy.force Ansi_color.stderr_supports_color ->
    [ "-fdiagnostics-color=always" ]
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

let get_flags ~for_ ctx =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx in
  (match for_ with
   | Compile version -> base_cxx_compile_flags version
   | Link -> base_cxx_link_flags)
    ccomp_type
;;
