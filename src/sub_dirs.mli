open Stdune

type 'set t = private
  { sub_dirs : 'set
  ; data_only : 'set
  }

module Status : sig
  type t = Ignored | Data_only | Normal
end

val default : Predicate_lang.t t

val make
  :  sub_dirs:Predicate_lang.t option
  -> ignored_sub_dirs:Predicate_lang.t list
  -> data_only:Predicate_lang.t option
  -> Predicate_lang.t t

val ignore_dirs
  :  Predicate_lang.t t
  -> dirs:Predicate_lang.t
  -> Predicate_lang.t t

val eval : Predicate_lang.t t -> dirs:string list -> String.Set.t t

val status : String.Set.t t -> dir:string -> Status.t
