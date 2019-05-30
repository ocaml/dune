(** Provide build information *)

module V1 : sig
  (** The version at which the current executable was built. *)
  val version : string

  module Statically_linked_library : sig
    type t

    (** The most visible name of the library. If it is a public
        library, this its public name otherwise it is its private name.
    *)
    val name : t -> string

    val version : t -> string
  end

  (** All the libraries that where statically linked in *)
  val statically_linked_libraries : Statically_linked_library.t list
end
