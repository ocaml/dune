(** Restrict the set of visible packages *)

open Import

module Clflags : sig
  type t =
    | No_restriction
    | Restrict of
        { names : Package.Name.Set.t
        ; command_line_option : string
              (** Which of [-p], [--only-packages], ... was passed *)
        }

  (** This must be called exactly once *)
  val set : t -> unit
end

(** Returns the filtered set of packages. This function is memoized. *)
val get : unit -> Package.t Package.Name.Map.t Memo.t

(** Returns the package restrictions. This function is memoized. *)
val get_mask : unit -> Package.t Package.Name.Map.t option Memo.t

(** Apply the package mask to the packages defined by the project *)
val packages_of_project : Dune_project.t -> Package.t Package.Name.Map.t Memo.t

(** Apply the package mask to the stanzas in the workspace *)
val filtered_stanzas : Context.t -> Dune_file.t list Memo.t

val stanzas_in_dir : Path.Build.t -> Dune_file.t option Memo.t
