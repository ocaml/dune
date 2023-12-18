open Import

(** Depend on an alias recursively. Return [Defined] if the alias is defined in
    at least one directory, and [Not_defined] otherwise. *)
val dep_on_alias_rec
  :  Alias.Name.t
  -> Path.Build.t
  -> Alias_builder.Alias_status.t Action_builder.t
