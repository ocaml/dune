open Stdune

module Dir : sig
  type 'a t

  val path : 'a t -> Path.Source.t

  val files : 'a t -> Filename.Set.t

  val file_paths : 'a t -> Path.Source.Set.t

  type 'a sub_dir

  val sub_dirs : 'a t -> 'a sub_dir Filename.Map.t

  val sub_dir_as_t : 'a sub_dir -> 'a t Memo.t

  val to_dyn : 'a t -> Dyn.t

  val value : 'a t -> 'a

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) : sig
    (** Traverse sub-directories recursively, pass them to [f] and combine
        intermediate results into a single one via [M.combine]. *)
    val map_reduce : 'a t -> f:('a t -> Outcome.t M.t) -> Outcome.t M.t
  end

  val sub_dir_names : 'a t -> Filename.Set.t
end

module Make
    (Reduced_stats : Reduced_stats_intf.S)
    (_ : Readdir.S with type reduced_stats := Reduced_stats.t) : sig
  type 'a t

  val create : 'a -> 'a t

  val find_dir : 'a t -> Path.Source.t -> 'a Dir.t option Memo.t

  val root : 'a t -> 'a Dir.t Memo.t

  (** [nearest_dir t fn] returns the directory with the longest path that is an
      ancestor of [fn]. *)
  val nearest_dir : 'a t -> Path.Source.t -> 'a Dir.t Memo.t

  (** [true] iff the path is a file *)
  val file_exists : 'a t -> Path.Source.t -> bool Memo.t

  val files_of : 'a t -> Path.Source.t -> Path.Source.Set.t Memo.t

  val dir_exists : 'a t -> Path.Source.t -> bool Memo.t
end
