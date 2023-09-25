open Stdune

let limit_output output ~n ~message =
  let noutput = String.length output in
  if noutput <= n
  then output
  else (
    match
      ( String.index_from output (n / 2) '\n'
      , String.rindex_from output (noutput - 1 - (n / 2)) '\n' )
    with
    | Some i1, Some i2 when i1 < i2 ->
      String.concat
        ~sep:""
        [ String.take output (i1 + 1); "..."; message; "..."; String.drop output i2 ]
    | _ ->
      let message = "\n..." ^ message ^ "...\n" in
      let output = String.take output (n - String.length message |> max 0) in
      output ^ message)
;;
