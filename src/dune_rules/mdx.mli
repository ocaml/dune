(** MDX integration *)

open Import

type t

val enabled_if : t -> Blang.t

include Stanza.S with type t := t

(** Generates the rules to handle the given mdx stanza *)
val gen_rules
  :  t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> unit Memo.t
