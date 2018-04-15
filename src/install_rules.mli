module type Params = sig
  val sctx : Super_context.t
end

module Archives (P : Params) : sig
  val lib_archive : Jbuild.Library.t -> dir:Path.t -> ext:string -> Path.t
  val stubs_archive : Jbuild.Library.t -> dir:Path.t -> Path.t
  val dll : Jbuild.Library.t -> dir:Path.t -> Path.t
end

module type Install_params = sig
  include Params
  val module_names_of_lib : Jbuild.Library.t -> dir:Path.t -> Module.t list
  val mlds_of_dir : Jbuild.Documentation.t -> dir:Path.t -> Path.t list
end

(** Generate install rules for META and .install files *)
module Gen (P : Install_params) : sig
  val init : unit -> unit
end
