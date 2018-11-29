open Stdune

type 'set t = private
  { dirs : 'set
  ; data_only : 'set
  }

module Status : sig
  type t = Ignored | Data_only | Normal
end

val default : Predicate_lang.t t

val add_data_only_dirs
  :  Predicate_lang.t t
  -> dirs:String.Set.t
  -> Predicate_lang.t t

val eval : Predicate_lang.t t -> dirs:string list -> String.Set.t t

val status : String.Set.t t -> dir:string -> Status.t

val decode : (Predicate_lang.t t * Dune_lang.Ast.t list) Stanza.Decoder.t
