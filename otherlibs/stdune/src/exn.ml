module String = Stdlib.StringLabels

type t = exn

external raise : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise : exn -> _ = "%reraise"

let protectx x ~f ~finally =
  match f x with
  | y ->
    finally x;
    y
  | exception e ->
    finally x;
    raise e
;;

let protect ~f ~finally = protectx () ~f ~finally

let pp_uncaught ~backtrace fmt exn =
  let s =
    Printf.sprintf "%s\n%s" (Printexc.to_string exn) backtrace
    |> String_split.split_lines
    |> List.map ~f:(Printf.sprintf "| %s")
    |> String.concat ~sep:"\n"
  in
  let line = String.make 71 '-' in
  Format.fprintf
    fmt
    "/%s\n| @{<error>Internal error@}: Uncaught exception.\n%s\n\\%s@."
    line
    s
    line
;;

let pp exn = Pp.text (Printexc.to_string exn)
let raise_with_backtrace = Printexc.raise_with_backtrace
let equal = ( = )
let hash = Stdlib.Hashtbl.hash
let to_dyn exn = Dyn.String (Printexc.to_string exn)
