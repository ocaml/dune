open Stdune

type t = private
  { path : Path.Source.t
  ; files : Filename.Set.t
  ; dirs : (Filename.t * Path.Source.t * File.t) list
  }

val empty : Path.Source.t -> t

module type S = sig
  type reduced_stats

  val stat :
       Path.Outside_build_dir.t
    -> (reduced_stats, Unix_error.Detailed.t) result Memo.t

  val readdir : Path.Source.t -> (t, Unix_error.Detailed.t) result Memo.t
end

module Make
    (Reduced_stats : Reduced_stats_intf.S) (Dir_contents : sig
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
    end) : S with type reduced_stats := Reduced_stats.t
