open Import

module Pkg : sig
  val decode : Package.t Dune_lang.Decoder.t

  val resolve :
    Dune_project.t -> Package.Name.t -> (Package.t, User_message.t) Result.t

  val field : string -> Package.t Dune_lang.Decoder.fields_parser

  val default_exn : loc:Loc.t -> Dune_project.t -> string -> Package.t
end

val modules_field : string -> Ordered_set_lang.t Dune_lang.Decoder.fields_parser
