open Stdune

include module type of struct
  include Cmdliner.Arg
end

module Path : sig
  module External : sig
    type t

    val path : t -> Path.External.t
    val arg : t -> string
  end

  type t

  val path : t -> Path.t
  val arg : t -> string
end

module Dep : sig
  type t = Dune_lang.Dep_conf.t

  val file : string -> t
  val alias : dir:Stdune.Path.Local.t -> Dune_engine.Alias.Name.t -> t
  val alias_rec : dir:Stdune.Path.Local.t -> Dune_engine.Alias.Name.t -> t
  val to_string_maybe_quoted : t -> string
end

val bytes : int64 conv
val context_name : Dune_engine.Context_name.t conv
val dep : Dep.t conv
val graph_format : Dune_graph.Graph.File_format.t conv
val path : Path.t conv
val external_path : Path.External.t conv
val package_name : Dune_rules.Package.Name.t conv
val profile : Dune_lang.Profile.t conv
val lib_name : Dune_lang.Lib_name.t conv
val version : Dune_lang.Syntax.Version.t conv
