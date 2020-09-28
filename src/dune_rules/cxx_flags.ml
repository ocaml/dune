open! Stdune
open Ocaml_config.Ccomp_type

let base_cxx_flags =
  [ (Gcc, [ "-x"; "c++"; "-lstdc++"; "-shared-libgcc" ])
  ; (Clang, [ "-x"; "c++" ])
  ; (Msvc, [ "/TP" ])
  ]

let get_flags ccomp_type =
  List.assoc_opt ccomp_type base_cxx_flags |> Option.value ~default:[]
