open Import
open Decoder

module Pkg : sig
  val decode : Package.t t
  val resolve : Dune_project.t -> Package.Name.t -> (Package.t, User_message.t) Result.t
  val field : stanza:string -> Package.t fields_parser
  val field_opt : ?check:unit t -> unit -> Package.t option fields_parser
  val default_exn : loc:Loc.t -> Dune_project.t -> string -> Package.t
end
