open Import

type 'a t =
  { mutable used : ('a * Loc.t list) Module.Name.Map.t
  }

let create () =
  { used = Module.Name.Map.empty
  }

let acknowledge t part ~loc ~modules =
  let already_used =
    Module.Name.Map.merge modules t.used ~f:(fun _name x l ->
      Option.some_if (Option.is_some x && Option.is_some l) ())
    |> Module.Name.Map.keys
    |> Module.Name.Set.of_list
  in
  t.used <-
    Module.Name.Map.merge modules t.used ~f:(fun _name x y ->
      match x with
      | None -> y
      | Some _ ->
        Some (part,
              loc :: match y with
              | None -> []
              | Some (_, l) -> l));
  already_used

let find t name = Option.map (Module.Name.Map.find t.used name) ~f:fst

let emit_warnings t =
  Module.Name.Map.iteri t.used ~f:(fun name (_, locs) ->
    match locs with
    | [] | [_] -> ()
    | loc :: _ ->
      let loc = Loc.in_file loc.start.pos_fname in
      Loc.warn loc
        "Module %a is used in several stanzas:@\n\
         @[<v>%a@]@\n\
         @[%a@]@\n\
         This warning will become an error in the future."
        Module.Name.pp_quote name
        (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
        locs
        Format.pp_print_text
        "To remove this warning, you must specify an explicit \"modules\" \
         field in every library, executable, and executables stanzas in \
         this jbuild file. Note that each module cannot appear in more \
         than one \"modules\" field - it must belong to a single library \
         or executable.")
