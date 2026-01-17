open Import

(** Convert opam commands to dune actions.

    Takes a variable lookup function, a location, the package, and
    a list of opam commands. Returns the corresponding dune actions. *)
val to_actions
  :  (Package_variable_name.t -> Variable_value.t option)
  -> Loc.t
  -> OpamPackage.t
  -> OpamTypes.command list
  -> (Dune_lang.Action.t list, User_message.t) result
