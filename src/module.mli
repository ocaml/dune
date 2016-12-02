open! Import

type t =
  { name      : string (** Name of the module. This is always the basename of the filename
                           without the extension. *)
  ; ml_fname  : string
  ; mli_fname : string option (** Object name. It is different from [name] for wrapped
                                  modules. *)
  ; obj_name  : string
  }

(** Real unit name once wrapped. This is always a valid module name. *)
val real_unit_name : t -> string

val file      : t -> dir:Path.t -> Ml_kind.t -> Path.t option
val cm_source : t -> dir:Path.t -> Cm_kind.t -> Path.t option
val cm_file   : t -> dir:Path.t -> Cm_kind.t -> Path.t
val cmt_file  : t -> dir:Path.t -> Ml_kind.t -> Path.t option
