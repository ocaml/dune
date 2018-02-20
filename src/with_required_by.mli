(** Value with a dependency path *)

module Entry : sig
  type t =
    | Path of Path.t
    | Alias of Path.t
    | Library of Path.t * string
    | Preprocess of string list
    | Loc of Loc.t

  (** [jbuild_file_in ~dir = Path (Path.relative dir "jbuild")] *)
  val jbuild_file_in : dir:Path.t -> t

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Entries : sig
  type t = Entry.t list

  val pp : Format.formatter -> t -> unit
end

type 'a t =
  { data        : 'a
  ; required_by : Entries.t
  }

(** Add an entry at the beginning the [required_by] stack *)
val prepend_one : 'a t -> Entry.t -> 'a t

(** Add entries at the end of the [required_by] stack *)
val append : 'a t -> Entry.t list -> 'a t

(** Re-raise an exception and augment it's dependency path with the given entry. The
    raised exception will be wrapped. *)
val reraise : exn -> Entry.t -> _

(** Extract a wrapped exception *)
val unwrap_exn : exn -> exn * Entry.t list option
