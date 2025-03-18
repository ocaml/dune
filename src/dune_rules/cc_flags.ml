open Import

type cc_vendor =
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
  | Gcc | Clang -> [ "-fdiagnostics-color=always" ]
  | _ -> []
;;

let warnings = function
  | Gcc | Clang -> [ "-Wall" ]
  | Msvc -> [ "-W2" ]
  | _ -> []
;;

let preprocessed_filename = "cc_vendor"

let cc_vendor build_dir =
  let open Action_builder.O in
  let filepath =
    Path.Build.(relative (relative build_dir ".dune/cc_vendor") preprocessed_filename)
  in
  let+ cc_vendor = Action_builder.contents (Path.build filepath) in
  match String.trim cc_vendor with
  | "clang" -> Clang
  | "gcc" -> Gcc
  | "msvc" -> Msvc
  | s -> Other s
;;

let check_warn = function
  | Other s ->
    User_warning.emit
      [ Pp.textf
          "Dune was not able to automatically infer the C/C++ compiler in use: \"%s\". \
           Please open an issue on GitHub to help us improve this feature."
          s
      ]
  | _ -> ()
;;

let cc_vendor (ctx : Build_context.t) =
  let open Action_builder.O in
  let+ cc_vendor = cc_vendor ctx.build_dir in
  check_warn cc_vendor;
  cc_vendor
;;

let get_flags ~for_ ctx =
  let open Action_builder.O in
  let+ cc_vendor = cc_vendor ctx in
  (match for_ with
   | Compile version -> base_cxx_compile_flags version
   | Link -> base_cxx_link_flags)
    cc_vendor
;;
