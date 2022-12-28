open Import

type phase =
  | Compile
  | Link

type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

let base_cxx_flags ~for_ cc =
  match (cc, for_) with
  | Gcc, Compile -> [ "-x"; "c++" ]
  | Gcc, Link -> [ "-lstdc++"; "-shared-libgcc" ]
  | Clang, Compile -> [ "-x"; "c++" ]
  | Clang, Link -> [ "-lc++" ]
  | Msvc, Compile -> [ "/TP" ]
  | Msvc, Link -> []
  | Other _, (Link | Compile) -> []

let fdiagnostics_color = function
  | (Gcc | Clang) when Lazy.force Ansi_color.stderr_supports_color ->
    [ "-fdiagnostics-color=always" ]
  | _ -> []

let preprocessed_filename = "ccomp"

let ccomp_type build_dir =
  let open Action_builder.O in
  let filepath =
    Path.Build.(
      relative (relative build_dir ".dune/ccomp") preprocessed_filename)
  in
  let+ ccomp = Action_builder.contents (Path.build filepath) in
  match String.trim ccomp with
  | "clang" -> Clang
  | "gcc" -> Gcc
  | "msvc" -> Msvc
  | s -> Other s

let check_warn = function
  | Other s ->
    User_warning.emit
      [ Pp.textf
          "Dune was not able to automatically infer the C compiler in use: \
           \"%s\". Please open an issue on github to help us improve this \
           feature."
          s
      ]
  | _ -> ()

let ccomp_type ctx =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx.Context.build_dir in
  check_warn ccomp_type;
  ccomp_type

let get_flags ~for_ ctx =
  let open Action_builder.O in
  let+ ccomp_type = ccomp_type ctx in
  base_cxx_flags ~for_ ccomp_type
