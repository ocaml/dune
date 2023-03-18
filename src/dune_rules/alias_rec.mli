open Import

module Alias_status : sig
  type t =
    | Defined
    | Not_defined

  include Monoid.S with type t := t
end

val dep_on_alias_rec :
     project:Dune_project.t
  -> Alias.Name.t
  -> Path.Build.t
  -> Alias_status.t Action_builder.t
