(** Generic logger adapter *)

(** Dune_util contains a Log interface that is tightly coupled to dune - it
    requires the build dir to be set before logging. Some libraries exported in
    dune-private-libs need to use that logger when used from within dune, but
    should still work as a library, and allow for reusing the client logger and
    not require any notion of build directory. *)

type 'a logger =
  ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

(** The following recipe is the adaptor for the widely used Logs module.

    {[ let f x = x ]} *)

(** Abstract logger interface that can accomodate both Dune_util logger and
    external ones. By defaulting it to the Dune_util.Log.Logger module, one can
    transparently use the internal logger from within dune and allow client code
    to insert their own. *)
module type S = sig
  val debug : 'a logger

  val err : 'a logger

  val info : 'a logger

  val warn : 'a logger
end

(** The following recipe is the adaptor for the widely used Logs module.

    {[
      module Log : Stdune.Log.S = struct
        let conv f m =
          let m = m ?header:None ?tags:None in
          f m

        let debug f = Logs.debug (conv f)

        let err f = Logs.err (conv f)

        let info f = Logs.info (conv f)

        let warn f = Logs.warn (conv f)
      end
    ]} *)
