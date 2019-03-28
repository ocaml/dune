open! Stdune

type t

(** The source_root directory *)
val dir : t -> Path.t

(** The directory for ocamldep files *)
val obj_dir : t -> Path.t

(** The private compiled native file directory *)
val native_dir : t -> Path.t

(** The private compiled byte file directories, and all cmi *)
val byte_dir : t -> Path.t

val all_cmis: t -> Path.t list

(** The public compiled cmi file directory *)
val public_cmi_dir: t -> Path.t

val need_dedicated_public_dir : t -> bool

val pp: t Fmt.t
val to_sexp: t -> Sexp.t

val all_obj_dirs : t -> mode:Mode.t -> Path.t list

val make_lib
  :  dir:Path.t
  -> has_private_modules:bool
  -> Lib_name.Local.t
  -> t

val make_exe: dir:Path.t -> name:string -> t

val make_external_no_private : dir:Path.t -> t

val encode : t -> Dune_lang.t list
val decode : dir:Path.t -> t Dune_lang.Decoder.t

val convert_to_external : t -> dir:Path.t -> t

val cm_dir : t -> Cm_kind.t -> Visibility.t -> Path.t

val cm_public_dir : t -> Cm_kind.t -> Path.t
