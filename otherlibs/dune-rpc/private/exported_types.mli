(** Types exposed to end-user consumers of [dune_rpc.mli]. *)

module Loc : sig
  type t = Stdune.Lexbuf.Loc.t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

  val start : t -> Lexing.position
  val stop : t -> Lexing.position
  val sexp : t Conv.value
end

(** This is kept around for compatibility reasons. Before we serialised [Pp.t] tags as
    [(Tag pp)] but now we serialise them as [Tag (pair tag pp)]. *)
val sexp_pp_unit : unit Pp.t Conv.value

module Target : sig
  type t =
    | Path of string
    | Alias of string
    | Library of string
    | Executables of string list
    | Preprocess of string list
    | Loc of Loc.t

  val sexp : t Conv.value
end

module Path : sig
  type t = string

  val dune_root : t
  val absolute : string -> t
  val relative : t -> string -> t
  val to_string_absolute : t -> string
  val sexp : t Conv.value
end

module Ansi_color : sig
  module RGB8 : sig
    type t = Stdune.Ansi_color.RGB8.t

    val to_int : t -> int
    val of_int : int -> t
    val sexp : t Conv.value
  end

  module RGB24 : sig
    type t = Stdune.Ansi_color.RGB24.t

    val to_int : t -> int
    val of_int : int -> t
    val red : t -> int
    val green : t -> int
    val blue : t -> int
    val make : red:int -> green:int -> blue:int -> t
    val sexp : t Conv.value
  end

  module Style : sig
    type t = Stdune.Ansi_color.Style.t

    val sexp : t Conv.value
  end
end

module User_message : sig
  module Style : sig
    type t = Stdune.User_message.Style.t =
      | Loc
      | Error
      | Warning
      | Kwd
      | Id
      | Prompt
      | Hint
      | Details
      | Ok
      | Debug
      | Success
      | Ansi_styles of Ansi_color.Style.t list
  end

  type t = Stdune.User_message.t

  (** (De)serializer for [User_message.t] which ignores the [annots] field. The
      [annots] field is non-trivial to serialize and is not necessary for
      formatting messages, so it's not handled here. *)
  val sexp_without_annots : t Conv.value
end

module Diagnostic : sig
  type severity =
    | Error
    | Warning

  module Promotion : sig
    type t =
      { in_build : string
      ; in_source : string
      }

    val in_build : t -> string
    val in_source : t -> string
    val sexp : t Conv.value
  end

  module Id : sig
    type t

    val compare : t -> t -> Ordering.t
    val hash : t -> int
    val create : int -> t
    val sexp : t Conv.value
  end

  module Related : sig
    type t =
      { message : User_message.Style.t Pp.t
      ; loc : Loc.t
      }

    val message : t -> unit Pp.t
    val message_with_style : t -> User_message.Style.t Pp.t
    val loc : t -> Loc.t
    val sexp : t Conv.value
  end

  type t =
    { targets : Target.t list
    ; id : Id.t
    ; message : User_message.Style.t Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    ; related : Related.t list
    }

  val related : t -> Related.t list
  val id : t -> Id.t
  val loc : t -> Loc.t option
  val message : t -> unit Pp.t
  val message_with_style : t -> User_message.Style.t Pp.t
  val severity : t -> severity option
  val promotion : t -> Promotion.t list
  val targets : t -> Target.t list
  val directory : t -> string option
  val to_dyn : t -> Dyn.t
  val to_user_message : t -> Stdune.User_message.t

  module Event : sig
    type nonrec t =
      | Add of t
      | Remove of t

    val to_dyn : t -> Dyn.t
    val sexp : t Conv.value
  end

  val sexp : t Conv.value
end

module Progress : sig
  type t =
    | Waiting
    | In_progress of
        { complete : int
        ; remaining : int
        ; failed : int
        }
    | Failed
    | Interrupted
    | Success

  val sexp : t Conv.value
end

module Message : sig
  type t =
    { payload : Csexp.t option
    ; message : string
    }

  val payload : t -> Csexp.t option
  val message : t -> string
  val sexp : t Conv.value
  val to_sexp_unversioned : t -> Csexp.t
end

module Job : sig
  module Id : sig
    type t

    val compare : t -> t -> Ordering.t
    val hash : t -> int
    val create : int -> t
    val sexp : t Conv.value
  end

  type t =
    { id : Id.t
    ; pid : int
    ; description : unit Pp.t
    ; started_at : float
    }

  val id : t -> Id.t
  val pid : t -> int
  val description : t -> unit Pp.t
  val started_at : t -> float

  module Event : sig
    type nonrec t =
      | Start of t
      | Stop of Id.t

    val sexp : t Conv.value
  end
end

(** A compound user error defines an alternative format for error messages that
    retains more structure. This can be used to display the errors in richer
    form by RPC clients. *)
module Compound_user_error : sig
  type t = private
    { main : User_message.t
    ; related : User_message.t list
    }

  val sexp : t Conv.value
  val to_dyn : t -> Dyn.t
  val annot : t list Stdune.User_message.Annots.Key.t
  val make : main:User_message.t -> related:User_message.t list -> t
  val parse_output : dir:Stdune.Path.t -> string -> t list
end

module Build_outcome_with_diagnostics : sig
  type t =
    | Success
    | Failure of Compound_user_error.t list

  val sexp_v1 : t Conv.value
  val sexp_v2 : t Conv.value
  val sexp : t Conv.value
end

(** Describe what files should be promoted. The second argument of [These] is a
    function that is called on files that cannot be promoted. *)
module Files_to_promote : sig
  type t =
    | All
    | These of Stdune.Path.Source.t list * (Stdune.Path.Source.t -> unit)

  val sexp : t Conv.value
end
