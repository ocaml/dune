type formatter := Format.formatter
type 'a t = formatter -> 'a -> unit

val pp_str_list : string list t
val sexp : formatter -> string -> 'a t -> 'a -> unit
val str : string t
val optint : int option t
val opt : 'a t -> 'a option t
val char : char t
val bool : bool t
val int : int t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val list : ?pp_sep:unit t -> 'a t -> 'a list t
val bytes : Bytes.t t
val array : ?pp_sep:unit t -> 'a t -> 'a array t
val lit : string -> unit t
val to_to_string : 'a t -> 'a -> string
val quoted_string : string t
