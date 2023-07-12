open Import

module Best_path : sig
  module Make
      (Monad : Monad.S) (_ : sig
        val file_exists : Path.t -> bool Monad.t
      end) : sig
    (** [best_path ~dir prog] if [prog] is one of the special programs that can
        be installed with an .opt extension, look it up in [dir] using this
        extension *)
    val best_path : dir:Path.t -> Filename.t -> Path.t option Monad.t
  end

  val memo : dir:Path.t -> Filename.t -> Path.t option Memo.t
end

(** [which ~path prog] finds the path of [prog] in [path] *)
val which : path:Path.t list -> Filename.t -> Path.t option Memo.t
