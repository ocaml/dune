(** MDX integration *)
open! Dune_engine

open Stdune

type t

type Stanza.t += T of t

(** Genrates the rules to handle the given mdx stanza *)
val gen_rules :
  t -> sctx:Super_context.t -> dir:Path.Build.t -> expander:Expander.t -> unit
