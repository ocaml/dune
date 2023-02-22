open Stdune

(** Lazily loaded file system.

    The set of modules in this library are designed to represent the source tree
    of a build system. *)

module type Reduced_stats = sig
  (** We functorize over this type to avoid pulling the entire engine *)

  type t =
    { st_dev : int  (** Device number *)
    ; st_ino : int  (** Inode number *)
    ; st_kind : File_kind.t  (** Kind of the file *)
    }
end

module Make (Reduced_stats : Reduced_stats) : sig
  module Readdir : sig
    (** Memoized interaction with the directories in the file system. *)

    type t

    module type S = sig
      val stat :
           Path.Outside_build_dir.t
        -> (Reduced_stats.t, Unix_error.Detailed.t) result Memo.t

      val readdir :
        Path.Source.t -> (Readdir.t, Unix_error.Detailed.t) result Memo.t
    end

    (** Construct a directory loading function. *)
    module Make (Dir_contents : sig
      type t

      (** The sorted list of file names with kinds. *)
      val to_list : t -> (Filename.t * File_kind.t) list
    end) (_ : sig
      val path_stat :
           Path.Outside_build_dir.t
        -> (Reduced_stats.t, Unix_error.Detailed.t) result Memo.t

      val dir_contents :
           Path.Outside_build_dir.t
        -> (Dir_contents.t, Unix_error.Detailed.t) result Memo.t
    end) : S
  end

  module Source_tree : sig
    (** Lazily loaded trie based on a directory reading and a stat loading
        function *)
    module Dir : sig
      type 'a t

      val path : _ t -> Path.Source.t

      val files : _ t -> Filename.Set.t

      val file_paths : _ t -> Path.Source.Set.t

      type 'a sub_dir

      val sub_dirs : 'a t -> 'a sub_dir Filename.Map.t

      val sub_dir_as_t : 'a sub_dir -> 'a t Memo.t

      val to_dyn : _ t -> Dyn.t

      module Make_map_reduce (M : Memo.S) (Outcome : Monoid) : sig
        (** Traverse sub-directories recursively, pass them to [f] and combine
            intermediate results into a single one via [M.combine]. *)
        val map_reduce : 'a t -> f:('a t -> Outcome.t M.t) -> Outcome.t M.t
      end

      val sub_dir_names : _ t -> Filename.Set.t
    end

    module Make (_ : Readdir.S) : sig
      type 'a t

      val find_dir : 'a t -> Path.Source.t -> 'a Dir.t option Memo.t

      val root : 'a t -> 'a Dir.t Memo.t

      (** [nearest_dir t fn] returns the directory with the longest path that is
          an ancestor of [fn]. *)
      val nearest_dir : 'a t -> Path.Source.t -> 'a Dir.t Memo.t

      (** [true] iff the path is a file *)
      val file_exists : 'a t -> Path.Source.t -> bool Memo.t

      val files_of : 'a t -> Path.Source.t -> Path.Source.Set.t Memo.t

      val dir_exists : 'a t -> Path.Source.t -> bool Memo.t
    end
  end
end
