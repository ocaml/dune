open Stdune
open Dune

include module type of struct
  include Cmdliner.Arg
end

module Path : sig
  type t

  val path : t -> Path.t

  val arg : t -> string
end

module Dep : sig
  type t = Dune_file.Dep_conf.t

  val file : string -> t

  val alias : string -> t

  val alias_rec : string -> t

  val to_string_maybe_quoted : t -> string
end

val dep : Dep.t conv

val path : Path.t conv

val package_name : Package.Name.t conv

val profile : Profile.t conv
