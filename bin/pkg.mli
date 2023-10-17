open Import

module Lock : sig
  (** [pp_packages lock_dir] returns a list of pretty-printed packages
      occuring in [lock_dir]. *)
  val pp_packages : Dune_pkg.Lock_dir.Pkg.t Package_name.Map.t -> 'a Pp.t
end

val group : unit Cmd.t
