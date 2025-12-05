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
  ; rocqdoc_header : Path.t option
  ; rocqdoc_footer : Path.t option
  }

val default : t
val dump : dir:Path.t -> t -> Dune_lang.t list
