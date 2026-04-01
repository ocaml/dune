open Stdune

module Write_result : sig
  type t =
    | Ok
    | Already_present
    | Error of exn
end

(** Write a given [content] to a temporary file in [Layout.temp_dir], and then
    atomically move it to a specified destination.

    If the destination already exists, return [Already_present].

    When the [mode] is set to [Copy], there is a small chance that atomicity is
    violated, in which case the destination is silently overwritten and the
    function returns [Ok] instead of [Already_present]. *)
val write_atomically : mode:Mode.t -> content:string -> Path.t -> Write_result.t

(** The functions in this module are bare wrappers that assume that the "target
    directory" (whatever that means for a given function) already exists. If the
    wrapped function fails, then the "target directory" is created, and the
    wrapped function called again.

    The objective is to call [Path.mkdir_p] only when needed, as it entails an
    additional system call. When this module was first introduced, [mkdir_p] was
    much more expensive (one system call per path component), so the benefit is
    much smaller now. *)
module Optimistically : sig
  (** Wrapper around [Path.rename]. *)
  val rename : src:Path.t -> dst:Path.t -> unit

  (** Wrapper around [Path.link]. *)
  val link : src:Path.t -> dst:Path.t -> unit
end
