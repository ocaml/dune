(** Emit warnings that respect dune's conventions *)

open Import

module Context : sig
  (** a context decides whether a warning should be emitted based on the
      settings inside the project and if the directory we're in is vendored *)
  type t

  val project : Dune_project.t -> t
  val source_dir_or_enable : Path.Source.t option -> t
end

(** [emit w ctx f] will call [f] if [w] is enabled in [ctx]. [f] should
    generate the warning corresponding to [w] *)
val emit : Warning.t -> Context.t -> (unit -> User_message.t Memo.t) -> unit Memo.t

val emit_project : Warning.t -> Dune_project.t -> User_message.t -> unit

module Bag : sig
  (** A set of warnings collected while parsing the dune language *)
  type t

  val create : unit -> t
  val decode : Warning.t -> (unit -> User_message.t Memo.t) -> unit Dune_sexp.Decoder.t

  (**  [set t decoder] make [decoder] record warnings by adding them to [t] *)
  val set : t -> 'a Dune_sexp.Decoder.t -> 'a Dune_sexp.Decoder.t

  (** [emit_all t] returns all the warnings collected in [decode] *)
  val emit_all : t -> unit Memo.t
end
