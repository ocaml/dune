type re

type match_info =
  | Match of Group.t
  | Failed
  | Running of { no_match_starts_before : int }

val match_str_no_bounds
  :  groups:bool
  -> partial:bool
  -> re
  -> string
  -> pos:int
  -> len:int
  -> match_info

val match_str
  :  groups:bool
  -> partial:bool
  -> re
  -> string
  -> pos:int
  -> len:int
  -> match_info

val match_str_p : re -> string -> pos:int -> len:int -> bool
val compile : Ast.t -> re
val group_count : re -> int
val group_names : re -> (string * int) list
val pp_re : re Fmt.t
