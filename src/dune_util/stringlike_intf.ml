open Stdune

module type S_base = sig
  type t

  (** The name of the module, for use in error messages. For example
      ["Lib_name"], ["Context_name"]. *)
  val module_ : string

  (** A short description of the type, for use in user-facing error messages.
      For example "context name", "library name". *)
  val description : string

  (** A description of valid identifiers. Will be added to error messages if
      present *)
  val description_of_valid_string : 'a Pp.t option

  (** A function suggesting a valid replacement for an erroneous input. Will be
      added to error messages if present *)
  val hint_valid : (string -> string) option

  val of_string_opt : string -> t option
  val to_string : t -> string
end

module type S = sig
  (** An interface for types that are convertible from/to strings. *)
  type t

  (** [of_string] should only be used on strings that are known to represent a
      valid [t]. *)
  val of_string : string -> t

  val to_string : t -> string
  val parse_string_exn : Loc.t * string -> t
  val to_dyn : t -> Dyn.t
  val of_string_opt : string -> t option
  val of_string_user_error : Loc.t * string -> (t, User_message.t) result

  (** From&to string conversions, for use with [Cmdliner.Arg.conv] *)
  val conv : (string -> (t, [> `Msg of string ]) result) * (Format.formatter -> t -> unit)

  include Dune_sexp.Conv.S with type t := t

  val decode_loc : (Loc.t * t) Dune_sexp.Decoder.t
end
