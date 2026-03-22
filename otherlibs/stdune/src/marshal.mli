val to_string : 'a -> sharing:bool -> string
val to_channel : out_channel -> 'a -> sharing:bool -> unit
val from_channel : in_channel -> 'a
val from_string : string -> 'a
