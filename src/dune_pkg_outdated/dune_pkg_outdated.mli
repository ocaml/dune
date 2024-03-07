open Import

(** Library for finding and printing outdated packges in the dune_pkg lock directory. *)

(** [t] represents the result of searching for outdated packages in a lock directory. *)
type t

(** [find ~repos ~local_packages packages] searches for outdated packages in the given
    collection of [packages] by consulting the [repos] and [local_packages].*)
val find
  :  repos:Opam_repo.t list
  -> local_packages:Dune_pkg.Local_package.t Package_name.Map.t
  -> Lock_dir.Pkg.t Package_name.Map.t
  -> t Fiber.t

(** [pp t ~transitive ~lock_dir_path] returns a specially constructed user message
    explaining the outdated packages. It begins with a summary detailing the number of
    affected packages and then lists all the packages requested.

    - [transitive] indicates whether to hint that transitive dependencies are not being
      shown and therefore the user should pass [--transitive] to the [dune pkg outdated]
      command to see them.

    - [lock_dir_path] is the path to the lock directory that will appear in the messages. *)
val pp : t -> transitive:bool -> lock_dir_path:Path.Source.t -> User_message.Style.t Pp.t

val packages_that_were_not_found : t -> Package_name.t list

module For_tests : sig
  (** Special module for internal testing only. *)

  type result

  val package_is_best_candidate : result

  val better_candidate
    :  is_immediate_dep_of_local_package:bool
    -> name:string
    -> newer_version:Package_version.t
    -> outdated_version:Package_version.t
    -> result

  val explain_results
    :  result list
    -> transitive:bool
    -> lock_dir_path:Path.Source.t
    -> User_message.Style.t Pp.t list

  val pp
    :  result list
    -> transitive:bool
    -> lock_dir_path:Path.Source.t
    -> User_message.Style.t Pp.t
end
