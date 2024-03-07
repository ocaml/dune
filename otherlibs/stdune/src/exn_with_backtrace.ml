type t =
  { exn : exn
  ; backtrace : Printexc.raw_backtrace
  }

let capture exn = { exn; backtrace = Printexc.get_raw_backtrace () }

let try_with f =
  match f () with
  | r -> Result.Ok r
  | exception exn -> Error (capture exn)
;;

let try_with_never_returns f =
  match f () with
  | (_ : Nothing.t) -> .
  | exception exn -> capture exn
;;

let reraise { exn; backtrace } = Exn.raise_with_backtrace exn backtrace

let pp_uncaught fmt { exn; backtrace } =
  Exn.pp_uncaught ~backtrace:(Printexc.raw_backtrace_to_string backtrace) fmt exn
;;

let pp { exn; backtrace } =
  let open Pp.O in
  Exn.pp exn
  ++ Pp.newline
  ++ Pp.text "backtrace:"
  ++ Pp.newline
  ++ Pp.text (Printexc.raw_backtrace_to_string backtrace)
;;

let map { exn; backtrace } ~f = { exn = f exn; backtrace }
let map_and_reraise t ~f = reraise (map ~f t)

let to_dyn { exn; backtrace } =
  let open Dyn in
  record
    [ "exn", string (Printexc.to_string exn)
    ; "backtrace", string (Printexc.raw_backtrace_to_string backtrace)
    ]
;;
