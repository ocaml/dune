open Stdune
open Dune_rules
open Build_api.Api

include module type of struct
  include Cmdliner.Arg
end

module Path : sig
  type t

  val path : t -> Path.t

  val arg : t -> string
end

module Dep : sig
  type t = Dep_conf.t

  val file : string -> t

  val alias : dir:Stdune.Path.Local.t -> Build_api.Api.Alias.Name.t -> t

  val alias_rec : dir:Stdune.Path.Local.t -> Build_api.Api.Alias.Name.t -> t

  val to_string_maybe_quoted : t -> string
end

val bytes : int64 conv

val context_name : Context_name.t conv

val dep : Dep.t conv

val path : Path.t conv

val package_name : Package.Name.t conv

val profile : Profile.t conv

val lib_name : Lib_name.t conv

val version : Dune_lang.Syntax.Version.t conv
