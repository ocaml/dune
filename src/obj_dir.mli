open! Stdune

type t = private
  { dir: Path.t (** The main directory *)
  ; public_dir: Path.t  (** The public compiled file directory *)
  ; private_dir: Path.t option   (** The private compiled file directory *)
  }

val pp: t Fmt.t
val to_sexp: t -> Sexp.t

val all_objs_dir: t -> Path.t list

val make_local:
  dir: Path.t ->
  has_private_modules:bool ->
  Lib_name.Local.t ->
  t

val make_exe:
  dir: Path.t ->
  string ->
  t

val make_external:
  dir: Path.t ->
  t

val encode : t -> Dune_lang.t list
val decode : dir:Path.t -> t Dune_lang.Decoder.t
