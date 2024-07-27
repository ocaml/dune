include Stdlib.Lazy

let map t ~f = lazy (f (force t))
