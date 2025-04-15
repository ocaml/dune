(* dune/src/dune_project.ml *)

let known_fields = [ ":version"; ":with-test"; ":with-doc"; ":optional" ]

let is_similar a b =
  let open String in
  if length a <> length b then false
  else
    let diffs =
      fold_lefti (fun acc i ca -> if ca = b.[i] then acc else acc + 1) 0 a
    in
    diffs <= 2

let detect_typos sexp =
  let open Dune_sexp in
  let rec scan = function
    | List (Atom (A "package") :: xs) ->
        List.iter (function
          | List (Atom (A "depends") :: deps) ->
              List.iter (function
                | List (_name :: fields) ->
                    List.iter (function
                      | Atom (A field) when not (List.mem field known_fields) ->
                          List.iter (fun known ->
                            if is_similar field known then
                              Format.eprintf
                                "Warning: unknown field '%s'. Did you mean '%s'?@." 
                                field known
                          ) known_fields
                      | _ -> ()
                    ) fields
                | _ -> ()
              ) deps
          | _ -> ()
        ) xs
    | _ -> ()
  in
  scan sexp

(* Call detect_typos when parsing the dune-project file *)

let parse_project sexp =
  detect_typos sexp;
  (* existing logic to parse project *)
  ... (* your original parsing code here *)

