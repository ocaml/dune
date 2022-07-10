module Loc : sig
  (** Location of dependency **)

  type t

  (** Convert to [Stdune.Loc.t] *)
  val to_loc : t -> fname:string -> Stdune.Loc.t
end

module Module : sig
  (** Coq module dependency *)

  type t

  val loc : t -> Loc.t

  val name : t -> string
end

module Ml_module : sig
  (** OCaml module dependency *)

  type t

  val loc : t -> Loc.t

  val name : t -> string
end

module From : sig
  (** From prefix Require dependency *)

  type t

  val prefix : t -> Module.t option

  val require : t -> Module.t
end

module Load : sig
  (** Loading an external file dependency *)

  type t

  val loc : t -> Loc.t

  val path : t -> string
end

module ExtraDep : sig
  (** Extra dependency *)

  type t

  val loc : t -> Loc.t

  val from : t -> Module.t

  val file : t -> string
end

(** Aggregated dependencies for a coq module. *)
type t

(** From Require dependencies. *)
val froms : t -> From.t list

(** Declare ML modules *)
val declares : t -> Ml_module.t list

(** Load statements of explicit files. *)
val loads : t -> Load.t list

(** Extra dependencies for a coq module. *)
val extradeps : t -> ExtraDep.t list

(** Extract all dependencies from [lexbuf]. *)
val lexbuf : Lexing.lexbuf -> t

(** Convert aggregated dependency tokens to a [Csexp.t]. *)
val to_csexp : t -> Csexp.t

(** Read a [Csexp.t] into aggregated dependency tokens. *)
val of_csexp : Csexp.t -> t
