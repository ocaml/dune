open Stdune

let is_valid str =
  let len = String.length str in
  len > 0
  &&
  let rec loop ix =
    match str.[ix] with
    | '"'
    | '('
    | ')'
    | ';' ->
      true
    | '|' ->
      ix > 0
      &&
      let next = ix - 1 in
      str.[next] = '#' || loop next
    | '#' ->
      ix > 0
      &&
      let next = ix - 1 in
      str.[next] = '|' || loop next
    | ' '
    | '\t'
    | '\n'
    | '\012'
    | '\r' ->
      true
    | _ -> ix > 0 && loop (ix - 1)
  in
  not (loop (len - 1))
