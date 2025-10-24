open Import

module Include : sig
  type t = Loc.t * string

  include Stanza.S with type t := t
end

module Dynamic_include : sig
  type t = Include.t

  include Stanza.S with type t := t
end

val stanza_package : Stanza.t -> Package.Id.t option

(** [of_ast project ~dir ast] is the list of [Stanza.t]s derived from decoding
    the [ast] according to the syntax given by [kind] in the context of the
    [project] *)
val of_ast : Dune_project.t -> dir:Path.Source.t -> Dune_lang.Ast.t -> Stanza.t list
