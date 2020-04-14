(** MDX integration *)

open Stdune

type t

type Stanza.t += T of t

val gen_rules : t -> sctx:Super_context.t -> dir:Path.Build.t -> unit
(** Genrates the rules to handle the given mdx stanza *)
