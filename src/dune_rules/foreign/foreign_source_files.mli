open Import

(** A map from object names to the corresponding sources. *)
type t

val to_list_map : t -> f:(string -> Loc.t * Foreign_source.t -> 'b) -> 'b list
val make : (Loc.t * Foreign_source.t) String.Map.t -> t

val object_files
  :  t
  -> dir:Path.Build.t
  -> ext_obj:Filename.Extension.t
  -> Path.Build.t list

val has_cxx_sources : t -> bool
