include Stdlib.Seq

let map t ~f = map f t
let filter t ~f = filter f t
let filter_map t ~f = filter_map f t
let fold_left t ~init ~f = fold_left f init t
let iter t ~f = iter f t
let return t = return t
let cons x next () = Cons (x, next)

let rec append seq1 seq2 () =
  match seq1 () with
  | Nil -> seq2 ()
  | Cons (x, next) -> Cons (x, append next seq2)
;;

let rec concat seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> append x (concat next) ()
;;
