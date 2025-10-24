open Import

val decode : Package.t Decoder.t

val resolve
  :  Dune_project.t
  -> Package_mask.t
  -> Loc.t * Package.Name.t
  -> (Package.t, User_message.t) Result.t

val field : stanza:string -> Package.t Decoder.fields_parser
val field_opt : ?check:unit Decoder.t -> unit -> Package.t option Decoder.fields_parser
val default_exn : loc:Loc.t -> Dune_project.t -> string -> Package.t
