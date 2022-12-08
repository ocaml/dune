open Types

type t

val default : t

val find : t -> Method_name.t -> Method_version.t option

(** For each method known by both local and remote, choose the highest common
    version number. Returns [None] if the resulting menu would be empty. *)
val select_common :
     local_versions:Method_version.Set.t Method_name.Map.t
  -> remote_versions:(Method_name.t * Method_version.t list) list
  -> t option

val of_list :
     (Method_name.t * Method_version.t) list
  -> (t, Method_name.t * Method_version.t * Method_version.t) result

val to_list : t -> (Method_name.t * Method_version.t) list

val to_dyn : t -> Dyn.t
