open Stdune

type t

val bindings : t -> Pform.Map.t
val scope : t -> Scope.t
val dir : t -> Path.t
val ectx : t -> Action.Unexpanded.expansion_context

val make
  :  scope:Scope.t
  -> context:Context.t
  -> cxx_flags:string list
  -> t

val set_env : t -> var:string -> value:string -> t

val set_dir : t -> dir:Path.t -> t

val update
  :  t
  -> dir:Path.t
  -> scope:Scope.t
  -> env:Env.t
  -> add_bindings:Pform.Map.t
  -> t

val expand_var
  :  t
  -> (Value.t list, Pform.Expansion.t) result option String_with_vars.expander

val expand_var_exn : t -> Value.t list option String_with_vars.expander

val expand_static
  :  t
  -> mode:'a String_with_vars.Mode.t
  -> template:String_with_vars.t
  -> 'a
