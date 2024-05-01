open! Import
open! Stdune

module Version : sig
  type t

  val all_by_string : (string * t) list
end

(** Downloads, builds, and installs a compiler toolchain of a given
    version. Performs no actions if the specified toolchain is already
    installed. *)
val get : log:[ `Always | `Never | `Install_only ] -> Version.t -> unit Fiber.t
