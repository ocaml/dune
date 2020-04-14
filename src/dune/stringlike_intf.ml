open Import

module type S_base = sig
  type t

  val module_ : string
  (** The name of the module, for use in error messages. For example
      ["Lib_name"], ["Context_name"]. *)

  val description : string
  (** A short description of the type, for use in user-facing error messages.
      For example "context name", "library name". *)

  val description_of_valid_string : 'a Pp.t option
  (** A description of valid identifiers. Will be added to error messages if
      present *)

  val of_string_opt : string -> t option

  val to_string : t -> string
end

module type S = sig
  (** An interface for types that are convertible from/to strings. *)
  type t

  val of_string : string -> t
  (** [of_string] should only be used on strings that are known to represent a
      valid [t]. *)

  val to_string : t -> string

  val parse_string_exn : Loc.t * string -> t

  val to_dyn : t -> Dyn.t

  val of_string_opt : string -> t option

  val conv :
    (string -> (t, [> `Msg of string ]) result)
    * (Format.formatter -> t -> unit)
  (** From&to string conversions, for use with [Cmdliner.Arg.conv] *)

  include Dune_lang.Conv.S with type t := t

  val decode_loc : (Loc.t * t) Dune_lang.Decoder.t
end
