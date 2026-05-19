val parse : string -> (string option, string) result
(** [parse s] parses [s] and returns a filename or [None] if the filename
    is equivalent to [/dev/null].

    Returns [Error msg] in case of error. *)

(** {1 Git header parsers} *)

val parse_git_header_rename :
  from_:string -> to_:string -> string -> (string * string) option
(** [parse_git_header_rename ~from_ ~to_ str] will parse [str] by trying to
    match [from_] and [to_] on the left side and right side of the space split
    respectively. Returns [None] if nothing was able to be found. *)

val parse_git_header_same : string -> (string * string) option
(** [parse_git_header_same str] will parse [str] by trying to get the largest
    equal suffix for both filenames in the git header. Returns [None] if
    nothing was able to be found. *)
