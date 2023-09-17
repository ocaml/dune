module Diff : sig
  module Mode : sig
    type t =
      | Binary (** no diffing, just raw comparison *)
      | Text (** diffing after newline normalization *)
  end

  type nonrec ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; file1 : 'path
    ; file2 : 'target
    }

  val map : ('p, 't) t -> path:('p -> 'a) -> target:('t -> 'b) -> ('a, 'b) t
end

module Outputs : sig
  type t =
    | Stdout
    | Stderr
    | Outputs (** Both Stdout and Stderr *)

  val to_string : t -> string
end

module Inputs : sig
  type t = Stdin

  val to_string : t -> string
end

module File_perm : sig
  (** File mode, for when creating files. We only allow what Dune takes into
      account when memoizing commands. *)

  type t =
    | Normal
    | Executable

  val suffix : t -> string
  val to_unix_perm : t -> int
end
