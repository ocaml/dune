type 'a t = Format.formatter -> 'a -> unit

val list : ?pp_sep:unit t -> 'a t -> 'a list t

val text : string t

val ocaml_list : 'a t -> 'a list t

val nl : unit t
