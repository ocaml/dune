open Import

type t =
  { dir          : Path.t
  ; all_modules  : Module.t Module.Name.Map.t
  ; mutable used : Loc.t list Module.Name.Map.t
  }

let create ~dir ~all_modules =
  { dir
  ; all_modules
  ; used = Module.Name.Map.empty
  }

let acknowledge t ~loc ~modules =
  let already_used =
    Module.Name.Map.merge modules t.used ~f:(fun _name x l ->
      Option.some_if (Option.is_some x && Option.is_some l) ())
    |> Module.Name.Map.keys
    |> Module.Name.Set.of_list
  in
  t.used <-
    Module.Name.Map.merge modules t.used ~f:(fun _name x l ->
      match x with
      | None -> l
      | Some _ -> Some (loc :: Option.value l ~default:[]));
  already_used

let emit_warnings t =
  let loc =
    Utils.jbuild_file_in ~dir:t.dir
    |> Path.to_string
    |> Loc.in_file
  in
  Module.Name.Map.iteri t.used ~f:(fun name locs ->
    if List.length locs > 1 then
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
