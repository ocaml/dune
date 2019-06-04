open Dune_memory.DuneMemory
open Stdune

let parse_metadata s =
  let rec convert = function
    | Sexplib.Sexp.Atom x ->
        Stdune.Sexp.Atom x
    | Sexplib.Sexp.List l ->
        Stdune.Sexp.List (List.map ~f:convert l)
  in
  match Sexplib.Sexp.parse s with
  | Sexplib.Sexp.Done (exp, _) ->
      (* FIXME: check there's no leftover *)
      convert exp
  | Sexplib.Sexp.Cont _ ->
      raise (Failed (Printf.sprintf "unfinished sexp"))

let main () =
  let memory =
    make
      ~log:(Log.create ~path:(Path.of_string "/tmp/log") ())
      (Path.of_string Sys.argv.(1))
  and cmd = Sys.argv.(2) in
  match cmd with
  | "promote" ->
      let files = Array.sub Sys.argv ~pos:3 ~len:(Array.length Sys.argv - 4)
      and metadata = parse_metadata Sys.argv.(Array.length Sys.argv - 1) in
      let promotions =
        promote memory
          (Array.to_list
             (Array.map
                ~f:(fun p ->
                  let p = Path.of_string p in
                  (p, Digest.file p) )
                files))
          metadata None
      in
      ignore
        (List.map
           ~f:(fun p -> Printf.printf "%s\n" (promotion_to_string p))
           promotions)
  | "search" ->
      let metadata = parse_metadata Sys.argv.(3) in
      ignore
        (List.map
           ~f:(fun (sym, act) ->
             Printf.printf "%s: %s\n" (Path.to_string sym) (Path.to_string act)
             )
           (search memory metadata))
  | _ ->
      raise (Failed (Printf.sprintf "unkown command: %s" cmd))

let () =
  try main ()
  with Failed msg | Sys_error msg ->
    Printf.printf "%s: fatal error: %s\n" Sys.argv.(0) msg ;
    exit 1
