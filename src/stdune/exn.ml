module List = Dune_caml.ListLabels
module String = Dune_caml.StringLabels
module Dyn = Dyn0
type t = exn

exception Fatal_error of string

exception Loc_error of Loc0.t * string

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

let () =
  Printexc.register_printer (function
    | Loc_error (loc, s) -> Some (Format.asprintf "%a%s" Loc0.print loc s)
    | _ -> None)

let fatalf ?loc fmt =
  Format.ksprintf (fun s ->
    match loc with
    | None -> raise (Fatal_error s)
    | Some loc -> raise (Loc_error (loc, s))
  ) fmt

let protectx x ~f ~finally =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let protect ~f ~finally = protectx () ~f ~finally

let code_error message vars =
  List.map vars ~f:(fun (v, sexp) ->
    (v, Dyn0.Sexp sexp))
  |> Code_error.raise message

let pp_uncaught ~backtrace fmt exn =
  let s =
    Printf.sprintf "%s\n%s" (Printexc.to_string exn) backtrace
    |> String_split.split_lines
    |> ListLabels.map ~f:(Printf.sprintf "| %s")
    |> String.concat ~sep:"\n"
  in
  let line = String.make 71 '-' in
  Format.fprintf fmt
    "/%s\n\
     | @{<error>Internal error@}: Uncaught exception.\n\
     %s\n\
     \\%s@."
    line s line

let pp fmt exn =
  Format.pp_print_string fmt (Printexc.to_string exn)

include
  ((struct
    [@@@warning "-32-3"]
    let raise_with_backtrace exn _ = reraise exn
    include Printexc
    let raise_with_backtrace exn bt = raise_with_backtrace exn bt
  end) : (sig
     val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _
   end))

let equal = (=)
let hash = Dune_caml.Hashtbl.hash

let to_dyn exn = Dyn0.String (Printexc.to_string exn)
