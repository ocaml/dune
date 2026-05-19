open Import
open Memo.O

let which loc context ~path =
  (let which = Which.which ~path in
   match Sys.unix with
   | false -> which Filename.make
   | true ->
     which Filename.gmake
     >>= (function
      | None -> which Filename.make
      | Some _ as s -> Memo.return s))
  >>| function
  | Some p -> p
  | None -> Utils.program_not_found ~context ~loc:(Some loc) "make"
;;
