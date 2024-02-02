(** Restrict the set of visible packages *)

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

type t = Package.t Package.Name.Map.t option

val mask : Package.t Package.Name.Map.t -> vendored:Package.Name.Set.t -> t
val filter_packages : t -> Package.t Package.Name.Map.t -> Package.t Package.Name.Map.t
val filter_packages_in_project : vendored:bool -> Dune_project.t -> Dune_project.t
