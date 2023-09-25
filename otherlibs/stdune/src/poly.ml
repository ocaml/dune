let hash = Stdlib.Hashtbl.hash
let compare x y = Ordering.of_int (Stdlib.compare x y)
let equal = ( = )
let ( = ) = ( = )
let ( <> ) = ( <> )
let ( < ) = ( < )
let ( <= ) = ( <= )
let ( > ) = ( > )
let ( >= ) = ( >= )
