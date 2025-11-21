(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)
open Import

type t =
  { rocq_flags : string list
  ; rocqdep_flags : string list
  ; rocqdoc_flags : string list
  }

let default = { rocq_flags = [ "-q" ]; rocqdep_flags = []; rocqdoc_flags = [ "--toc" ] }

let dump { rocq_flags; rocqdep_flags; rocqdoc_flags } =
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "rocq_flags", rocq_flags
    ; "rocqdep_flags", rocqdep_flags
    ; "rocqdoc_flags", rocqdoc_flags
    ]
;;
