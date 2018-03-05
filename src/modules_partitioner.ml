open Import

type t =
  { dir          : Path.t
  ; all_modules  : Module.t String_map.t
  ; mutable used : Loc.t list String_map.t
  }

let create ~dir ~all_modules =
  { dir
  ; all_modules
  ; used = String_map.empty
  }

let acknowledge t ~loc ~modules =
  let already_used =
    String_map.merge modules t.used ~f:(fun _name x l ->
      Option.some_if (Option.is_some x && Option.is_some l) ())
    |> String_map.keys
    |> String_set.of_list
  in
  t.used <-
    String_map.merge modules t.used ~f:(fun _name x l ->
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
  String_map.iteri t.used ~f:(fun name locs ->
    if List.length locs > 1 then
      Loc.warn loc
        "Module %S is used in several stanzas:@\n\
         @[<v>%a@]@\n\
         @[%a@]@\n\
         This warning will become an error in the future."
        name
        (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
        locs
        Format.pp_print_text
        "To remove this warning, you must specify an explicit \"modules\" \
         field in every library, executable, and executables stanzas in \
         this jbuild file. Note that each module cannot appear in more \
         than one \"modules\" field - it must belong to a single library \
         or executable.")
