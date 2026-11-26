open Import

(** Source files and directories as seen by source loading. A workspace file is
    read from the workspace. A mounted file has a logical path, used for source
    semantics and diagnostics, and is read from an external or built backing. Relative includes must stay within both roots of their mount. *)

module Backing : sig
  type t =
    | Fs of Path.Outside_build_dir.t
    | Build of Path.Build.t

  val path : t -> Path.t
  val append_local : t -> Path.Local.t -> t
  val relative_fname : t -> Filename.t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t

  (** Built listings are unfiltered; external listings use package source
      filtering. Missing directories, including virtual subdirectories,
      return [Ok None]. *)
  val dir_contents
    :  t
    -> ( (Filename.Array.Set.t * Filename.Array.Set.t) option
         , Unix_error.Detailed.t )
         result
         Memo.t

  (** The listing of a sub-directory, or [Ok None] if one is missing. *)
  val find_dir
    :  t
    -> Filename.t list
    -> ( (Filename.Array.Set.t * Filename.Array.Set.t) option
         , Unix_error.Detailed.t )
         result
         Memo.t

  val unreadable : loc:Loc.t -> t -> Unix_error.Detailed.t -> 'a
end

module File : sig
  type t

  val source_path : t -> Path.Local.t

  (** The logical path used by source semantics and diagnostics. *)
  val path : t -> Path.t

  (** The path the file is read from. *)
  val backing_path : t -> Path.t

  val as_workspace : t -> Path.Source.t option
  val parent_exn : t -> t
  val relative : t -> Loc.t -> string -> t
  val exists : t -> bool Memo.t
  val read : t -> string option Memo.t
  val equal : t -> t -> bool
  val diagnostic_name : t -> string
  val with_lexbuf_from_file : t -> f:(Lexing.lexbuf -> 'a) -> 'a Memo.t
end

module Dir : sig
  type t = File.t

  val workspace : Path.Source.t -> t
  val mounted : loc:Loc.t -> logical_root:Path.Build.t -> backing_root:Backing.t -> t
  val source_path : t -> Path.Local.t
  val path : t -> Path.t
  val as_workspace : t -> Path.Source.t option
  val basename : t -> Filename.t
  val file : t -> Filename.t -> File.t

  (** Workspace listings are read as in the workspace tree; mounted listings
      come from the backing, and failing to read one is an error. *)
  val contents : t -> Dir_contents.t Memo.t
end
