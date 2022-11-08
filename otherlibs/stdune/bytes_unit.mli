(** Conversion table for byte suffixes and their corresponding [Int64.t] values.
    The first element of the tuple is a list of possible suffixes for the second
    element of the tuple which is the value. There are some static checks done
    on this table ensuring it is ordered and well-formed.*)
val conversion_table : (string list * Int64.t) list

val pp : Int64.t -> string
