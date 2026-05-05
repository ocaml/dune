module String : sig
  val is_prefix : prefix:string -> string -> bool
  val is_suffix : suffix:string -> string -> bool
  val cut : char -> string -> (string * string) option
  val cuts : char -> string -> string list
  val slice : ?start:int -> ?stop:int -> string -> string
  val count_common_suffix : string -> string -> int
end

module List : sig
  val last : 'a list -> 'a
end
