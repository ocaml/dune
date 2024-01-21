open Import

type t =
  { name : Loc.t * Lib_name.t
  ; package : Package.t
  ; sub_dir : string option
  }

(** Subdirectory inside the installation directory *)
val sub_dir : t -> string option

val loc : t -> Loc.t

(** Full public name *)
val name : t -> Lib_name.t

(** Package it is part of *)
val package : t -> Package.t

val make
  :  allow_deprecated_names:bool
  -> Dune_project.t
  -> Loc.t * Lib_name.t
  -> (t, User_message.t) result

val decode : allow_deprecated_names:bool -> t Dune_lang.Decoder.t
