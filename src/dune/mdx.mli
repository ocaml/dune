(** MDX integration *)

open Stdune

type t

type Stanza.t += T of t

(** Genrates the rules to handle the given mdx stanza *)
val gen_rules : sctx:Super_context.t -> dir:Path.Build.t -> t -> unit
