
type t = {
  exn : exn;
  backtrace : Printexc.raw_backtrace;
}

let capture exn =
  { exn; backtrace = Printexc.get_raw_backtrace () }

let try_with f x =
  match f x with
  | r -> Result.Ok r
  | exception exn ->
    Error (capture exn)

let reraise { exn; backtrace } =
  Exn.raise_with_backtrace exn backtrace

let pp_uncaught fmt { exn; backtrace } =
  Exn.pp_uncaught ~backtrace:(Printexc.raw_backtrace_to_string backtrace) fmt exn

let map { exn; backtrace } ~f =
  { exn = f exn; backtrace }

let map_and_reraise t ~f = reraise (map ~f t)
