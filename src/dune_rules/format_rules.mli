open Import

(** Setup format alias actions for the given dir. If tools like ocamlformat are
    not available in $PATH, just display an error message when the alias is
    built. *)
val setup_alias : Super_context.t -> dir:Path.Build.t -> unit Memo.t
