type 'a t = ('a, exn) Result.t

let equal eq x y = Result.equal eq Exn.equal x y
let hash h = Result.hash h Exn.hash
