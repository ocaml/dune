open Import
open Sexp.Of_sexp

type t =
  { name    : string
  ; version : string option
  }

let filename = "dune-project"

type lang =
  | Dune_0_1

let lang =
  let name =
    enum
      [ ("dune", ()) ]
  in
  let version ver =
    match string ver with
    | "0.1" -> Dune_0_1
    | _ ->
      of_sexp_error ver "unsupported version of the dune language"
  in
  field_multi "lang" (name @> version @> nil) (fun () v -> v)

let name ~dir =
  field_o "name" string >>= function
  | Some s -> return s
  | None -> return ("_" ^ String.concat ~sep:"_" (Path.explode_exn dir))

let parse ~dir =
  record
    (lang >>= fun Dune_0_1 ->
     name ~dir >>= fun name ->
     field_o "version" string >>= fun version ->
     return { name; version })

let load ~dir =
  let fname = Path.relative dir filename in
  let sexp = Io.Sexp.load_many_as_one fname in
  parse ~dir sexp
