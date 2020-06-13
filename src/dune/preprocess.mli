open Stdune

module Pps : sig
  type 'a t =
    { loc : Loc.t
    ; pps : 'a list
    ; flags : String_with_vars.t list
    ; staged : bool
    }

  val compare_no_locs : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
end

type 'a t =
  | No_preprocessing
  | Action of Loc.t * Action_dune_lang.t
  | Pps of 'a Pps.t
  | Future_syntax of Loc.t

module Without_instrumentation : sig
  type t = Loc.t * Lib_name.t

  val compare_no_locs : t -> t -> Ordering.t
end

val decode : Without_instrumentation.t t Dune_lang.Decoder.t

module Without_future_syntax : sig
  type 'a t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of 'a Pps.t
end

val loc : _ t -> Loc.t option

module Pp_flag_consumer : sig
  type t =
    | Compiler
    | Merlin
end

val remove_future_syntax :
     'a t
  -> for_:Pp_flag_consumer.t
  -> Ocaml_version.t
  -> 'a Without_future_syntax.t

module Per_module : sig
  type 'a preprocess = 'a t

  type 'a t = 'a preprocess Module_name.Per_item.t

  val decode : Without_instrumentation.t t Dune_lang.Decoder.t

  val no_preprocessing : unit -> 'a t

  val default : unit -> 'a t

  (** [find module_name] find the preprocessing specification for a given module *)
  val find : Module_name.t -> 'a t -> 'a preprocess

  val pps : Without_instrumentation.t t -> Without_instrumentation.t list

  val add_bisect : Without_instrumentation.t t -> Without_instrumentation.t t
end
with type 'a preprocess := 'a t
