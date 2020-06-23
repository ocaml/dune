type mkdir_p =
  | Already_exists
  | Created

val mkdir_p : ?perms:int -> string -> mkdir_p
