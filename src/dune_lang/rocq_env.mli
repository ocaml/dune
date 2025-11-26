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

(** Environment for Rocq. *)
type t

val equal : t -> t -> bool

(** Default environment for Rocq. *)
val default : t

(** Flags for Rocq binaries. *)
val flags : t -> Ordered_set_lang.Unexpanded.t

(** Flags for rocqdep *)
val rocqdep_flags : t -> Ordered_set_lang.Unexpanded.t

(** Flags for rocqdoc *)
val rocqdoc_flags : t -> Ordered_set_lang.Unexpanded.t

(** Rocqdoc header config. *)
val rocqdoc_header : t -> String_with_vars.t option

(** Rocqdoc footer config. *)
val rocqdoc_footer : t -> String_with_vars.t option

(** Parser for env stanza. *)
val decode : t Decoder.fields_parser
