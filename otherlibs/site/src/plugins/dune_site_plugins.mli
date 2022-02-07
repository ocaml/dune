module V1 : sig
  (** [load name] loads the given library *)
  val load : string -> unit

  (** [available name] tests if a library is available for loading. The loading
      can still fail if a dependency of the library is unavailable or if the
      loading of the modules fails. *)
  val available : string -> bool
end

(** **/ *)

module Private_ : sig
  module Plugins : module type of Plugins
  module Meta_parser : module type of Meta_parser
end
