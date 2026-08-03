type t = int

let create () = 1
let feed acc x = (acc * 31) + x
let hash t = t
