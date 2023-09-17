module List = Stdlib.ListLabels
module String = Stdlib.StringLabels
open String

let split s ~on =
  let rec loop i j =
    if j = length s
    then [ sub s ~pos:i ~len:(j - i) ]
    else if s.[j] = on
    then sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else loop i (j + 1)
  in
  loop 0 0
;;

let split_lines s =
  let rec loop ~last_is_cr ~acc i j =
    if j = length s
    then (
      let acc =
        if j = i || (j = i + 1 && last_is_cr)
        then acc
        else sub s ~pos:i ~len:(j - i) :: acc
      in
      List.rev acc)
    else (
      match s.[j] with
      | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
      | '\n' ->
        let line =
          let len = if last_is_cr then j - i - 1 else j - i in
          sub s ~pos:i ~len
        in
        loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
      | _ -> loop ~acc i (j + 1) ~last_is_cr:false)
  in
  loop ~acc:[] 0 0 ~last_is_cr:false
;;
