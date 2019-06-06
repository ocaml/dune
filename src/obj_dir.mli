open! Stdune

type 'path t

val of_local : Path.Build.t t -> Path.t t

(** The source_root directory *)
val dir : 'path t -> 'path

(** The directory for ocamldep files *)
val obj_dir : 'path t -> 'path

(** The private compiled native file directory *)
val native_dir : 'path t -> 'path

(** The private compiled byte file directories, and all cmi *)
val byte_dir : 'path t -> 'path

val all_cmis: 'path t -> 'path list

(** The public compiled cmi file directory *)
val public_cmi_dir: 'path t -> 'path

val all_obj_dirs : 'path t -> mode:Mode.t -> 'path list

val make_lib
  :  dir:Path.Build.t
  -> has_private_modules:bool
  -> Lib_name.Local.t
  -> Path.Build.t t

val make_external_no_private : dir:Path.t -> Path.t t

val encode : Path.t t -> Dune_lang.t list
val decode : dir:Path.t -> Path.t t Dune_lang.Decoder.t

val convert_to_external : Path.t t -> dir:Path.t -> Path.t t

val cm_dir : 'path t -> Cm_kind.t -> Visibility.t -> 'path

val cm_public_dir : 'path t -> Cm_kind.t -> 'path

val to_dyn : _ t -> Dyn.t

val make_exe: dir:Path.Build.t -> name:string -> Path.Build.t t

val as_local_exn : Path.t t -> Path.Build.t t

(** For local libraries with private modules, all public cmi's are symlinked to
    their own directory. Such a public cmi dir is only necessary if a library
    contains private modules *)
val need_dedicated_public_dir : Path.Build.t t -> bool
