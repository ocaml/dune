open Stdune

module type Params = sig
  val sctx : Super_context.t
end

module Archives (P : Params) : sig
  val lib_archive : Jbuild.Library.t -> dir:Path.t -> ext:string -> Path.t
  val stubs_archive : Jbuild.Library.t -> dir:Path.t -> Path.t
  val dll : Jbuild.Library.t -> dir:Path.t -> Path.t
end

(** Generate install rules for META and .install files *)
module Gen (P : Params) : sig
  val init : unit -> unit
end
