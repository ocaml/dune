open Import

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
  | Action of Loc.t * Action.t
  | Pps of 'a Pps.t
  | Future_syntax of Loc.t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t

module Without_instrumentation : sig
  type t = Loc.t * Lib_name.t

  val compare_no_locs : t -> t -> Ordering.t
end

module With_instrumentation : sig
  type t =
    | Ordinary of Without_instrumentation.t
    | Instrumentation_backend of
        { libname : Loc.t * Lib_name.t
        ; deps : Dep_conf.t list
        ; flags : String_with_vars.t list
        }

  val equal : t -> t -> bool
end

val decode : Without_instrumentation.t t Decoder.t

module Without_future_syntax : sig
  type 'a t =
    | No_preprocessing
    | Action of Loc.t * Action.t
    | Pps of 'a Pps.t
end

val loc : _ t -> Loc.t option

module Pp_flag_consumer : sig
  type t =
    | Compiler
    | Merlin
end

val remove_future_syntax
  :  'a t
  -> for_:Pp_flag_consumer.t
  -> Ocaml.Version.t
  -> 'a Without_future_syntax.t

module Instrumentation : sig
  type t =
    { backend : Loc.t * Lib_name.t
    ; flags : String_with_vars.t list
    ; deps : Dep_conf.t list
    ; loc : Loc.t
    }

  (** [instrumentation] multi field *)
  val instrumentation : t list Decoder.fields_parser
end

module Per_module : sig
  type 'a preprocess := 'a t
  type 'a t = 'a preprocess Module_name.Per_item.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val decode : Without_instrumentation.t t Decoder.t
  val no_preprocessing : unit -> 'a t
  val default : unit -> 'a t

  (** [find module_name] find the preprocessing specification for a given module *)
  val find : Module_name.t -> 'a t -> 'a preprocess

  val pps : Without_instrumentation.t t -> Without_instrumentation.t list

  val add_instrumentation
    :  With_instrumentation.t t
    -> Instrumentation.t
    -> With_instrumentation.t t

  val without_instrumentation : With_instrumentation.t t -> Without_instrumentation.t t
end

(** [preprocess] and [preprocessor_deps] fields *)
val preprocess_fields
  : (Without_instrumentation.t Per_module.t * Dep_conf.t list) Decoder.fields_parser
