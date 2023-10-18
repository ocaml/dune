open Import

module Lock : sig
  (** [pp_packages lock_dir] returns a list of pretty-printed packages
      occuring in [lock_dir]. *)
  val pp_packages : Dune_pkg.Lock_dir.Pkg.t list -> 'a Pp.t
end

val group : unit Cmd.t
