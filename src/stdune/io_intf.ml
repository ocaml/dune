module type S = sig
  type path

  val open_in : ?binary:bool (* default true *) -> path -> in_channel

  val open_out : ?binary:bool (* default true *) -> path -> out_channel

  val with_file_in :
    ?binary:bool (* default true *) -> path -> f:(in_channel -> 'a) -> 'a

  val with_file_out :
    ?binary:bool (* default true *) -> path -> f:(out_channel -> 'a) -> 'a

  val with_lexbuf_from_file : path -> f:(Lexing.lexbuf -> 'a) -> 'a

  val lines_of_file : path -> string list

  val read_file : ?binary:bool -> path -> string

  val write_file : ?binary:bool -> path -> string -> unit

  val compare_files : path -> path -> Ordering.t

  val compare_text_files : path -> path -> Ordering.t

  val write_lines : ?binary:bool -> path -> string list -> unit

  val copy_file : ?chmod:(int -> int) -> src:path -> dst:path -> unit -> unit

  val setup_copy :
       ?chmod:(int -> int)
    -> src:path
    -> dst:path
    -> unit
    -> in_channel * out_channel

  val file_line : path -> int -> string

  val file_lines : path -> start:int -> stop:int -> (string * string) list
end
