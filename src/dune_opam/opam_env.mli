open Import

(** Extend a filter environment to handle self-referencing variables.

    When evaluating filters for a package, the package's own name and version
    should be available as global variables. This function wraps an existing
    environment to add this behavior. *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamVariable.Full.t -> OpamVariable.variable_contents option)
  -> OpamVariable.Full.t
  -> OpamVariable.variable_contents option

(** Convert an opam environment update to a dune env update. *)
val opam_env_update_to_env_update
  :  string * OpamParserTypes.env_update_op * string * 'a
  -> String_with_vars.t Dune_lang.Action.Env_update.t
