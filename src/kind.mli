open Import

type 'a t =
  | String : string t
  | List   : 'a t -> 'a list t
  | Pair   : 'a t * 'b t -> ('a * 'b) t

val eq : 'a t -> 'b t -> ('a, 'b) eq

val to_sexp : 'a t -> 'a -> Sexp.t
val of_sexp : 'a t -> Sexp.t -> 'a

val load : 'a t -> filename:string -> 'a
val save : 'a t -> filename:string -> 'a -> unit
