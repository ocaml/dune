open Stdune

include Interned.Make(struct let resize_policy = Interned.Conservative end)
