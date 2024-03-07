(** We occasionally need to change the format and layout of the cache and some
    systems, e.g. the cache trimmer, provide support for all previous versions.

    Cache metadata and entries are versioned separately and this module keeps
    track of all historical versions. *)

module File : sig
  type t =
    | V3
    | V4

  val current : t
  val all : t list
  val to_string : t -> string
end

module Value : sig
  type t = V3

  val current : t
  val all : t list
  val to_string : t -> string
end

module Metadata : sig
  type t =
    | V3
    | V4
    | V5

  val current : t
  val all : t list
  val to_string : t -> string

  (** Metadata entries contain references to file entries. This function links
      the two versions. We guarantee that [file_version current = File.current]. *)
  val file_version : t -> File.t

  (** Like [file_version] but for value entries. *)
  val value_version : t -> Value.t
end
