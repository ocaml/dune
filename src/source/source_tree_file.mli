open Import

module Mounted_layer : sig
  type t =
    | Directory of
        { logical_root : Path.Build.t
        ; backing_root : Path.Build.t
        }
    | File of
        { logical : Path.Build.t
        ; backing : Path.Build.t
        }
    | Contents of
        { logical : Path.Build.t
        ; contents : string
        ; executable : bool
        ; dependencies : Path.t list
        }
    | Delete of { logical : Path.Build.t }

  val directory : logical_root:Path.Build.t -> backing_root:Path.Build.t -> t
  val file : logical:Path.Build.t -> backing:Path.Build.t -> t

  val contents
    :  logical:Path.Build.t
    -> contents:string
    -> executable:bool
    -> dependencies:Path.t list
    -> t

  val delete : logical:Path.Build.t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t
end

module File : sig
  type contents =
    { contents : string
    ; executable : bool
    ; dependencies : Path.t list
    }

  type materialization =
    | Copy of Path.t
    | Write of contents

  type t

  val workspace : Path.Source.t -> t
  val mounted : logical:Path.Build.t -> layers:Mounted_layer.t list -> t
  val source_path : t -> Source_path.t

  (** The logical path used by source semantics and diagnostics. *)
  val path : t -> Path.t

  (** The immutable input from which the file is read, when it has one. *)
  val backing_path_opt : t -> Path.t option Memo.t

  val backing_path : t -> Path.t Memo.t
  val as_workspace : t -> Path.Source.t option
  val relative : t -> Loc.t -> string -> t
  val materialization : t -> materialization option Memo.t
  val read_with_perm : t -> contents option Memo.t
  val read : t -> string option Memo.t
  val equal : t -> t -> bool
  val diagnostic_name : t -> string
  val to_dyn : t -> Dyn.t
end

module Dir : sig
  type t

  val workspace : Path.Source.t -> t

  val mounted
    :  logical:Path.Build.t
    -> backing:Path.Build.t
    -> layers:Mounted_layer.t list
    -> t

  val source_path : t -> Source_path.t

  (** The logical path used by source semantics. *)
  val path : t -> Path.t

  (** The immutable directory from which contents are enumerated. *)
  val backing_path : t -> Path.t

  val basename : t -> Filename.t
  val file : t -> Filename.t -> File.t
end
