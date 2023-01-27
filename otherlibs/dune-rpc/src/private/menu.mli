open Types

type t

val default : t

val find : t -> Method.Name.t -> Method.Version.t option

(** For each method known by both local and remote, choose the highest common
    version number. Returns [None] if the resulting menu would be empty. *)
val select_common :
     local_versions:Method.Version.Set.t Method.Name.Map.t
  -> remote_versions:(Method.Name.t * Method.Version.t list) list
  -> t option

val of_list :
     (Method.Name.t * Method.Version.t) list
  -> (t, Method.Name.t * Method.Version.t * Method.Version.t) result

val to_list : t -> (Method.Name.t * Method.Version.t) list

val to_dyn : t -> Dyn.t
