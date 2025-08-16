(** [replace ~all re ~f s] iterates on [s], and replaces every occurrence
    of [re] with [f substring] where [substring] is the current match.
    If [all = false], then only the first occurrence of [re] is replaced. *)
val replace
  :  ?pos:int (** Default: 0 *)
  -> ?len:int
  -> ?all:bool (** Default: true. Otherwise only replace first occurrence *)
  -> Compile.re (** matched groups *)
  -> f:(Group.t -> string) (** how to replace *)
  -> string (** string to replace in *)
  -> string

(** [replace_string ~all re ~by s] iterates on [s], and replaces every
    occurrence of [re] with [by]. If [all = false], then only the first
    occurrence of [re] is replaced.

    {5 Examples:}
    {[
      # let regex = Re.compile (Re.char ',');;
      val regex : re = <abstr>

      # Re.replace_string regex ~by:";" "[1,2,3,4,5,6,7]";;
      - : string = "[1;2;3;4;5;6;7]"

      # Re.replace_string regex ~all:false ~by:";" "[1,2,3,4,5,6,7]";;
      - : string = "[1;2,3,4,5,6,7]"
    ]} *)
val replace_string
  :  ?pos:int (** Default: 0 *)
  -> ?len:int
  -> ?all:bool (** Default: true. Otherwise only replace first occurrence *)
  -> Compile.re (** matched groups *)
  -> by:string (** replacement string *)
  -> string (** string to replace in *)
  -> string
