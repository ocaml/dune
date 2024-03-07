let equal eq x y = Result.equal eq Exn.equal x y
let hash h = Result.hash h Exn.hash
let to_dyn f = Result.to_dyn f Exn.to_dyn

type 'a t = ('a, exn) Result.t

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = Result.return
    let bind = Result.bind
  end)
