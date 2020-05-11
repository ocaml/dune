open Stdune

let int_of_string ?where s =
  match Int.of_string s with
  | Some s -> Ok s
  | None ->
    Result.Error
      (Printf.sprintf "invalid integer%s: %s"
         ( match where with
         | Some l -> " in " ^ l
         | None -> "" )
         s)

let int64_of_string ?where s =
  match Int64.of_string s with
  | res -> Ok res
  | exception _exn ->
    Result.Error
      (Printf.sprintf "invalid 64-bit integer%s: %s"
         ( match where with
         | Some l -> " in " ^ l
         | None -> "" )
         s)

let retry ?message ?(count = 100) f =
  let rec loop = function
    | x when x >= count ->
      Result.Error
        (Failure
           ( Printf.sprintf "too many retries (%i)" x
           ^
           match message with
           | None -> ""
           | Some msg -> ": " ^ msg ))
    | x -> (
      match f () with
      | Some v -> Result.Ok v
      | None ->
        Thread.delay 0.1;
        loop (x + 1) )
  in
  loop 0
