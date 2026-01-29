open Import

(** Filter conversion to Slang/Blang *)

val opam_raw_fident_to_slang : loc:Loc.t -> string -> (Slang.t, User_message.t) result

val opam_string_to_slang
  :  package:OpamPackage.t
  -> loc:Loc.t
  -> string
  -> (Slang.t, User_message.t) result

(** Translate an Opam filter into Dune's "Slang" Blang DSL. *)
val to_blang
  :  package:OpamPackage.t
  -> loc:Loc.t
  -> OpamTypes.filter
  -> (Slang.blang, User_message.t) result

(** Filter simplification using solver variable values. *)
val simplify
  :  (Package_variable_name.t -> Variable_value.t option)
  -> OpamTypes.filter
  -> OpamTypes.filter

(** Partial evaluation of filter. Returns [`Skip] if the filter evaluates
    to false, [`Filter filter] otherwise with the simplified filter. *)
val partial_eval
  :  OpamTypes.filter option
  -> [ `Filter of OpamTypes.filter option | `Skip ]
