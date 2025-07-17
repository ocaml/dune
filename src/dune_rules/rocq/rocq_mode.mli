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

(** We support different modes in order to speed up compilation:
    [VoOnly], where even if native is enabled, we will skip the
    compilation; [Native], meaning compile both .vo and "native" .cmxs
    files, [VosOnly], to generate only vos files. *)
type t =
  | VoOnly
  | Native
  | VosOnly

(** Note that we cannot decode [Native], as it is automatically
    inferred *)
val decode : t Dune_lang.Decoder.t
