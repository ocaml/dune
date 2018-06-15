open Import

module Version = struct
  type t = int * int

  let to_string (a, b) = sprintf "%u.%u" a b

  let sexp_of_t t = Sexp.unsafe_atom_of_string (to_string t)

  let t : t Sexp.Of_sexp.t = Sexp.Of_sexp.make (function
    | Atom (loc, A s) -> begin
        try
          Scanf.sscanf s "%u.%u" (fun a b -> (a, b))
        with _ ->
          Loc.fail loc "atom of the form NNN.NNN expected"
      end
    | sexp ->
      Sexp.Of_sexp.of_sexp_error sexp "atom expected")

  let can_read ~parser_version:(pa, pb) ~data_version:(da, db) =
    pa = da && db <= pb
end

module Versioned_parser = struct
  type 'a t = (int * 'a) Int.Map.t

  let make l =
    if List.is_empty l then
      Exn.code_error "Syntax.Versioned_parser.make got empty list" [];
    match
      List.map l ~f:(fun ((major, minor), p) -> (major, (minor, p)))
      |> Int.Map.of_list
    with
    | Ok x -> x
    | Error _ ->
      Exn.code_error
        "Syntax.Versioned_parser.make"
        [ "versions", Sexp.To_sexp.list Version.sexp_of_t (List.map l ~f:fst) ]

  let last t =
    let major, (minor, p) = Option.value_exn (Int.Map.max_binding t) in
    ((major, minor), p)

  let find_exn t ~loc ~data_version:(major, minor) =
    match
      Option.bind (Int.Map.find t major) ~f:(fun (minor', p) ->
        Option.some_if (minor' >= minor) p)
    with
    | None ->
      Loc.fail loc "Version %s is not supported.\n\
                    Supported versions:\n\
                    %s"
        (Version.to_string (major, minor))
        (String.concat ~sep:"\n"
           (Int.Map.to_list t |> List.map ~f:(fun (major, (minor, _)) ->
              sprintf "- %u.0 to %u.%u" major major minor)))
    | Some p -> p
end
