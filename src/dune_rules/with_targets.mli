open Import
module Action_builder := Dune_engine.Action_builder

type 'a build := 'a Dune_engine.Action_builder.t

type nonrec 'a t =
  { build : 'a Action_builder.t
  ; targets : Targets.t
  }

val map_build : 'a t -> f:('a build -> 'b build) -> 'b t
val return : 'a -> 'a t
val add : 'a t -> file_targets:Path.Build.t list -> 'a t
val add_directories : 'a t -> directory_targets:Path.Build.t list -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val write_file_dyn
  :  ?perm:Action.File_perm.t
  -> Path.Build.t
  -> string t
  -> Action.Full.t t

val all : 'a t list -> 'a list t

(** [memoize name t] is an action builder that behaves like [t] except that
    its result is computed only once. *)
val memoize : string -> 'a t -> 'a t

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end
