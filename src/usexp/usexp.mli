(** Parsing of s-expressions *)

module Atom : sig
  type t = private A of string [@@unboxed]
  (** Acceptable atoms are composed of chars in the range [' ' .. '~']
     and must be nonempty. *)

  val is_valid : string -> bool
  (** [is_valid s] checks that [s] respects the constraints to be an atom. *)

 val of_string : string -> t
  (** Convert a string to an atom.  If the string contains invalid
     characters, raise [Invalid_argument]. *)

  val of_int : int -> t
  val of_float : float -> t
  val of_bool : bool -> t
  val of_int64 : Int64.t -> t
  val of_digest : Digest.t -> t

  val to_string : t -> string
end

module Loc : sig
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }
end

(** The S-expression type *)
type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list

val atom : string -> t
(** [atom s] convert the string [s] to an Atom.
    @raise Invalid_argument if [s] does not satisfy [Atom.is_valid s]. *)

val atom_or_quoted_string : string -> t

val unsafe_atom_of_string : string -> t

(** Serialize a S-expression *)
val to_string : t -> string

(** Serialize a S-expression using indentation to improve readability *)
val pp : Format.formatter -> t -> unit

(** Same as [pp], but split long strings. The formatter must have been
    prepared with [prepare_formatter]. *)
val pp_split_strings : Format.formatter -> t -> unit

(** Prepare a formatter for [pp_split_strings]. Additionaly the
    formatter escape newlines when the tags "makefile-action" or
    "makefile-stuff" are active. *)
val prepare_formatter : Format.formatter -> unit

(** Abstract syntax tree *)
module Ast : sig
  type sexp = t
  type t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * t list

  val atom_or_quoted_string : Loc.t -> string -> t

  val loc : t -> Loc.t

  val remove_locs : t -> sexp

  module Token : sig
    type t =
      | Atom   of Loc.t * Atom.t
      | String of Loc.t * string
      | Lparen of Loc.t
      | Rparen of Loc.t
  end

  val tokenize : t -> Token.t list
end with type sexp := t

val add_loc : t -> loc:Loc.t -> Ast.t

module Parser : sig
  module Error : sig
    type t

    val position : t -> Lexing.position
    val message  : t -> string
  end

  (** Exception raised in case of a parsing error *)
  exception Error of Error.t

  module Mode : sig
    type sexp = t
    type 'a t =
      | Single : Ast.t t
      | Many   : Ast.t list t
  end with type sexp := t

  module Stack : sig
    (** Parser stack. The stack is not in [state] for optimization purposes. *)
    type t

    val empty : t
  end

  type 'a t

  (** Create a new parser state. [fname] is the filename the input is from. *)
  val create : fname:string -> mode:'a Mode.t -> 'a t

  (** Feed one character to the parser. In case of error, it raises [Parse_error] *)
  val feed : _ t -> char -> Stack.t -> Stack.t

  (** Instruct the parser that the end of input was reached. In case of error, it raises
      [Parse_error] *)
  val feed_eoi : 'a t -> Stack.t -> 'a

  (** {3 Convenience functions} *)

  val feed_string    : _ t -> string                       -> Stack.t -> Stack.t
  val feed_substring : _ t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes     : _ t -> bytes                        -> Stack.t -> Stack.t
  val feed_subbytes  : _ t -> bytes -> pos:int -> len:int  -> Stack.t -> Stack.t
end

val parse_string : fname:string -> mode:'a Parser.Mode.t -> string -> 'a
