open Stdune

module Pps : sig
  type t =
    { loc : Loc.t
    ; pps : (Loc.t * Lib_name.t) list
    ; flags : String_with_vars.t list
    ; staged : bool
    }

  val compare_no_locs : t -> t -> Ordering.t
end

type t =
  | No_preprocessing
  | Action of Loc.t * Action_dune_lang.t
  | Pps of Pps.t
  | Future_syntax of Loc.t

val decode : t Dune_lang.Decoder.t

module Without_future_syntax : sig
  type t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of Pps.t
end

val loc : t -> Loc.t option

module Pp_flag_consumer : sig
  type t =
    | Compiler
    | Merlin
end

val remove_future_syntax :
  t -> for_:Pp_flag_consumer.t -> Ocaml_version.t -> Without_future_syntax.t

module Per_module : sig
  type preprocess = t

  type t = preprocess Module_name.Per_item.t

  val decode : t Dune_lang.Decoder.t

  val no_preprocessing : t

  val default : t

  (** [find module_name] find the preprocessing specification for a given module *)
  val find : Module_name.t -> t -> preprocess

  val pps : t -> (Loc.t * Lib_name.t) list

  val add_bisect : t -> t
end
with type preprocess := t
