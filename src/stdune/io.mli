(** IO operations *)

val open_in  : ?binary:bool (* default true *) -> string -> in_channel
val open_out : ?binary:bool (* default true *) -> string -> out_channel

val close_in  : in_channel  -> unit
val close_out : out_channel -> unit

val with_file_in  : ?binary:bool (* default true *) -> string -> f:(in_channel -> 'a) -> 'a
val with_file_out : ?binary:bool (* default true *) -> string -> f:(out_channel -> 'a) -> 'a

val with_lexbuf_from_file : string -> f:(Lexing.lexbuf -> 'a) -> 'a

val lines_of_file : string -> string list

val read_file : string -> string
val write_file : string -> string -> unit

val compare_files : string -> string -> Ordering.t

val write_lines : string -> string list -> unit

val copy_channels : in_channel -> out_channel -> unit

val copy_file : src:string -> dst:string -> unit

val read_all : in_channel -> string
