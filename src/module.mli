open! Import

type t =
  { name      : string (** Name of the module. This is always the basename of the filename
                           without the extension. *)
  ; impl_fname : string
  ; intf_fname : string option

  ; obj_name  : string (** Object name. It is different from [name] for wrapped
                           modules. *)
  ; reason    : bool   (** Whether this a reason source*)
  }

(** Real unit name once wrapped. This is always a valid module name. *)
val real_unit_name : t -> string

val file      : t -> dir:Path.t -> Ml_kind.t -> Path.t option
val cm_source : t -> dir:Path.t -> Cm_kind.t -> Path.t option
val cm_file   : t -> dir:Path.t -> Cm_kind.t -> Path.t
val cmt_file  : t -> dir:Path.t -> Ml_kind.t -> Path.t option

val create
  : ?obj_name:string
  -> ?intf_fname:string
  -> name:string
  -> impl_fname:string
  -> unit -> t
val ocaml_of_reason : t -> t
