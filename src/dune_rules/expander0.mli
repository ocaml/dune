open Import

val isn't_allowed_in_this_position : source:Dune_lang.Template.Pform.t -> 'a
val as_in_build_dir : what:string -> loc:Loc.t -> Path.t -> Path.Build.t

module type S = sig
  type t

  val expand
    :  t
    -> mode:('deferred, 'value) String_with_vars.Mode.t
    -> String_with_vars.t
    -> 'value Action_builder.t

  val project : t -> Dune_project.t
  val eval_blang : t -> Blang.t -> bool Memo.t
  val expand_str : t -> String_with_vars.t -> string Action_builder.t
  val expand_str_partial : t -> String_with_vars.t -> String_with_vars.t Action_builder.t
end

include S

val expand_str_and_build_deps : t -> String_with_vars.t -> string Memo.t
val set_db : (dir:Path.Build.t -> t Memo.t) -> unit
val get : dir:Path.Build.t -> t Memo.t
val project : t -> Dune_project.t Memo.t
val create : 'a Memo.t -> (module S with type t = 'a) -> t
