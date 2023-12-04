type 'a t = 'a -> bool

let create x = x
let true_ _ = true
let false_ _ = false
let test f x = f x
let contramap t ~f x = t (f x)
