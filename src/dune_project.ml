open Import
open Sexp.Of_sexp

type t =
  { name : string
  }

let filename = "dune-project"

type lang =
  | Dune_0_1

let lang =
  let version ver =
    match string ver with
    | "0.1" -> Dune_0_1
    | _ ->
      of_sexp_error ver "unsupported version of the dune language"
  in
  let name =
    enum
      [ ("dune", ()) ]
  in
  sum
    [ cstr "lang" (name @> version @> nil) (fun () v -> v) ]

module Acc = struct
  type t =
    { name : string option
    }

  let init =
    { name = None }
end

let load ~dir =
  let fname = Path.relative dir filename in
  let sexps = Io.Sexp.load fname ~mode:Many in
  let langs, sexps =
    List.partition_map sexps ~f:(function
      | List (loc, Atom (_, A "lang") :: _) as sexp ->
        Left (lang sexp, loc)
      | sexp -> Right sexp)
  in
  let _lang =
    match langs with
    | [] ->
      Loc.fail (Loc.in_file (Path.to_string fname))
        "language not specified, you need to add (lang dune 0.1)"
    | [(v, _)] -> v
    | _ :: (_, loc) :: _ ->
      Loc.fail loc "language specified too many times"
  in
  let acc =
    List.fold_left sexps ~init:Acc.init ~f:(fun (acc : Acc.t) sexp ->
      sum
        [ cstr "lang" nil acc
        ; cstr_loc "name" (string @> nil) (fun loc name ->
            match acc.name with
            | None -> { Acc.name = Some name }
            | Some _ -> Loc.fail loc "name specified too many times")
        ]
        sexp)
  in
  { name =
      match acc.name with
      | Some s -> s
      | None -> "_" ^ String.concat ~sep:"_" (Path.explode_exn dir)
  }
