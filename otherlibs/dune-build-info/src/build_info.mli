(** See {!V1} for the current version. *)

module V1 : sig
  (** Provide build-time information.

      The entry points in this module are {!version} and
      {!Statically_linked_libraries.to_list}.

      {e Implementation note:} this module is implemented using special support
      from Dune. When an executable is linked, a special "blank" placeholder is
      stored as a string. A special post-link phase called
      {e artifact substitution} can replace this placeholder with encoded data
      that will be decoded by this library.

      Artifact substitution happens when an executable is installed or promoted
      to the source tree. *)

  module Version : sig
    (** Version numbers. *)

    type t

    val to_string : t -> string
  end

  (** The version at which the current executable was built.

      The version is [None] during development, it is only [Some _] once
      artifact substitution happened. *)
  val version : unit -> Version.t option

  module Statically_linked_library : sig
    (** A library with an optional version number. *)

    type t

    (** The most visible name of the library. If it is a public library, this
        its public name otherwise it is its private name. *)
    val name : t -> string

    val version : t -> Version.t option
  end

  module Statically_linked_libraries : sig
    (** Entry points to find {!Statically_linked_library} values. *)

    (** All the libraries that where statically linked in *)
    val to_list : unit -> Statically_linked_library.t list

    (** Find a particular library by name. *)
    val find : name:string -> Statically_linked_library.t option
  end
end
