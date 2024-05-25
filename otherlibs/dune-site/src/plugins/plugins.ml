open Dune_site.Private_
module Data = Dune_site_plugins_data

let meta_fn = "META"

let readdir =
  let ( / ) = Filename.concat in
  let readdir_noexn dir =
    try Sys.readdir dir with
    | Sys_error _ -> [||]
  in
  fun dirs ->
    List.concat
      (List.map
         (fun dir ->
           List.filter
             (fun entry -> Sys.file_exists (dir / entry / meta_fn))
             (Array.to_list (readdir_noexn dir)))
         dirs)
;;

let rec lookup dirs file =
  match dirs with
  | [] -> None
  | dir :: dirs ->
    let file' = Filename.concat dir file in
    if Sys.file_exists file' then Some file' else lookup dirs file
;;

module type S = sig
  val paths : string list
  val list : unit -> string list
  val load_all : unit -> unit
  val load : string -> unit
end

let rec check_predicates predicates =
  match Sys.backend_type, predicates with
  | _, [] -> true
  | Sys.Native, Meta_parser.Pos "byte" :: _ -> false
  | Sys.Bytecode, Meta_parser.Pos "native" :: _ -> false
  | Sys.Native, Meta_parser.Pos "native" :: predicates -> check_predicates predicates
  | Sys.Bytecode, Meta_parser.Pos "byte" :: predicates -> check_predicates predicates
  | Sys.Native, Meta_parser.Neg "native" :: _ -> false
  | Sys.Bytecode, Meta_parser.Neg "byte" :: _ -> false
  | Sys.Native, Meta_parser.Neg "byte" :: predicates -> check_predicates predicates
  | Sys.Bytecode, Meta_parser.Neg "native" :: predicates -> check_predicates predicates
  | _, Meta_parser.Pos pred :: predicates ->
    Data.findlib_predicates_set_by_dune pred && check_predicates predicates
  | _, Meta_parser.Neg pred :: predicates ->
    (not (Data.findlib_predicates_set_by_dune pred)) && check_predicates predicates
;;

let check_predicates_with_plugin predicates =
  let rec aux predicates has_plugin acc =
    match predicates with
    | [] -> has_plugin && check_predicates acc
    | Meta_parser.Pos "plugin" :: predicates -> aux predicates true acc
    | predicate :: predicates -> aux predicates has_plugin (predicate :: acc)
  in
  aux predicates false []
;;

let rec get_plugin plugins requires entries =
  match entries with
  | [] -> List.rev plugins, List.rev requires
  | Meta_parser.Comment _ :: entries -> get_plugin plugins requires entries
  | Package _ :: entries -> get_plugin plugins requires entries
  | Rule { var = "plugin"; predicates; action = Set; value } :: entries
    when check_predicates predicates -> get_plugin [ value ] requires entries
  | Rule { var = "plugin"; predicates; action = Add; value } :: entries
    when check_predicates predicates -> get_plugin (value :: plugins) requires entries
  (* archive(native|byte,plugin) is the way used in the wild before findlib
     supported plugins *)
  | Rule { var = "archive"; predicates; action = Set; value } :: entries
    when check_predicates_with_plugin predicates -> get_plugin [ value ] requires entries
  | Rule { var = "archive"; predicates; action = Add; value } :: entries
    when check_predicates_with_plugin predicates ->
    get_plugin (value :: plugins) requires entries
  | Rule { var = "requires"; predicates; action = Set; value } :: entries
    when check_predicates predicates -> get_plugin plugins [ value ] entries
  | Rule { var = "requires"; predicates; action = Add; value } :: entries
    when check_predicates predicates -> get_plugin plugins (value :: requires) entries
  | Rule _ :: entries -> get_plugin plugins requires entries
;;

exception Thread_library_required_by_plugin_but_not_required_by_main_executable

exception
  Library_not_found of
    { search_paths : string list
    ; prefix : string list
    ; name : string
    }

exception
  Plugin_not_found of
    { search_paths : string list
    ; name : string
    }

let () =
  Printexc.register_printer (function
    | Thread_library_required_by_plugin_but_not_required_by_main_executable ->
      Some
        (Format.asprintf
           "%a"
           Format.pp_print_text
           "It is not possible to dynamically link a plugin which uses the thread \
            library with an executable not already linked with the thread library.")
    | Plugin_not_found { search_paths; name } ->
      Some
        (Format.sprintf
           "The plugin %S can't be found in the search paths %S."
           name
           (String.concat ":" search_paths))
    | Library_not_found { search_paths; prefix = []; name } ->
      Some
        (Format.sprintf
           "The library %S can't be found in the search paths %S."
           name
           (String.concat ":" search_paths))
    | Library_not_found { search_paths; prefix; name } ->
      Some
        (Format.sprintf
           "The sub-library %S can't be found in the library %s in the search paths %S."
           name
           (String.concat "." prefix)
           (String.concat ":" search_paths))
    | _ -> None)
;;

let rec find_library ~dirs ~prefix ~suffix directory meta =
  let rec find_directory directory = function
    | [] -> directory
    | Meta_parser.Rule { var = "directory"; predicates = []; action = Set; value } :: _ ->
      (match directory with
       | None -> Some value
       | Some old -> Some (Filename.concat old value))
    | _ :: entries -> find_directory directory entries
  in
  match suffix with
  | [] -> find_directory directory meta, meta
  | pkg :: suffix ->
    let directory = find_directory directory meta in
    let rec aux pkg = function
      | [] ->
        raise
          (Library_not_found { search_paths = dirs; prefix = List.rev prefix; name = pkg })
      | Meta_parser.Package { name = Some name; entries } :: _ when String.equal name pkg
        -> find_library ~dirs ~prefix:(pkg :: prefix) ~suffix directory entries
      | _ :: entries -> aux pkg entries
    in
    aux pkg meta
;;

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = String.length s
    then []
    else if is_word_char s.[i]
    then parse_word i (i + 1)
    else skip_blanks (i + 1)
  and parse_word i j =
    if j = String.length s
    then [ StringLabels.sub s ~pos:i ~len:(j - i) ]
    else if is_word_char s.[j]
    then parse_word i (j + 1)
    else StringLabels.sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0
;;

let extract_comma_space_separated_words s =
  extract_words s ~is_word_char:(function
    | ',' | ' ' | '\t' | '\n' -> false
    | _ -> true)
;;

let split_all l = List.concat (List.map extract_comma_space_separated_words l)

let find_plugin ~dirs ~dir ~suffix (meta : Meta_parser.t) =
  let directory, meta =
    find_library ~dirs ~prefix:(Option.to_list meta.name) ~suffix None meta.entries
  in
  let plugins, requires = get_plugin [] [] meta in
  let directory =
    match directory with
    | None -> dir
    | Some pkg_dir ->
      if pkg_dir.[0] = '+' || pkg_dir.[0] = '^'
      then
        Filename.concat
          (Lazy.force Helpers.stdlib)
          (String.sub pkg_dir 1 (String.length pkg_dir - 1))
      else if Filename.is_relative pkg_dir
      then Filename.concat dir pkg_dir
      else pkg_dir
  in
  let plugins = split_all plugins in
  let requires = split_all requires in
  directory, plugins, requires
;;

let load file ~pkg =
  let entries =
    let ic = open_in file in
    try
      let lb = Lexing.from_channel ic in
      lb.lex_curr_p <- { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      let r = Meta_parser.Parse.entries lb 0 [] in
      close_in ic;
      r
    with
    | exn ->
      close_in ic;
      raise exn
  in
  { Meta_parser.name = Some pkg; entries }
;;

let lookup_and_load_one_dir ~dir ~pkg =
  let meta_file = Filename.concat dir meta_fn in
  if Sys.file_exists meta_file
  then Some (load meta_file ~pkg)
  else (
    (* Alternative layout *)
    let dir = Filename.dirname dir in
    let meta_file = Filename.concat dir (meta_fn ^ "." ^ pkg) in
    if Sys.file_exists meta_file then Some (load meta_file ~pkg) else None)
;;

let split ~dirs name =
  match String.split_on_char '.' name with
  | [] -> raise (Library_not_found { search_paths = dirs; prefix = []; name })
  | pkg :: rest -> pkg, rest
;;

let lookup_and_summarize alldirs name =
  let pkg, suffix = split ~dirs:alldirs name in
  let rec loop dirs =
    match dirs with
    | [] ->
      List.assoc_opt pkg Data.builtin_library
      |> (function
       | None -> raise (Library_not_found { search_paths = alldirs; prefix = []; name })
       | Some meta ->
         find_plugin ~dirs:alldirs ~dir:(Lazy.force Helpers.stdlib) ~suffix meta)
    | dir :: dirs ->
      let dir = Filename.concat dir pkg in
      (match lookup_and_load_one_dir ~dir ~pkg with
       | None -> loop dirs
       | Some p -> find_plugin ~dirs:alldirs ~dir ~suffix p)
  in
  loop alldirs
;;

let loaded_libraries =
  lazy
    (let h = Hashtbl.create 10 in
     List.iter (fun s -> Hashtbl.add h s ()) Data.already_linked_libraries;
     h)
;;

let load_gen ~load_requires dirs name =
  let loaded_libraries = Lazy.force loaded_libraries in
  if not (Hashtbl.mem loaded_libraries name)
  then (
    if name = "threads"
    then raise Thread_library_required_by_plugin_but_not_required_by_main_executable;
    Hashtbl.add loaded_libraries name ();
    let directory, plugins, requires = lookup_and_summarize dirs name in
    List.iter load_requires requires;
    List.iter
      (fun p ->
        let file = Filename.concat directory p in
        Dune_site_backend.Linker.load file)
      plugins)
;;

let rec load_requires name = load_gen ~load_requires (Lazy.force Helpers.ocamlpath) name

let load_plugin plugin_paths name =
  match lookup plugin_paths (Filename.concat name meta_fn) with
  | None -> raise (Plugin_not_found { search_paths = plugin_paths; name })
  | Some meta_file ->
    let meta = load meta_file ~pkg:name in
    let plugins, requires = get_plugin [] [] meta.entries in
    assert (plugins = []);
    let requires = split_all requires in
    List.iter load_requires requires
;;

module Make (X : sig
    val paths : string list
  end) : S = struct
  include X

  let list () = List.sort String.compare (readdir paths)
  let load name = load_plugin paths name
  let load_all () = List.iter load (list ())
end

let load = load_requires

let available name =
  Hashtbl.mem (Lazy.force loaded_libraries) name
  ||
  let ocamlpath = Lazy.force Helpers.ocamlpath in
  try
    ignore (lookup_and_summarize ocamlpath name);
    true
  with
  | _ ->
    (* CR - What exceptions are being swallowed here? *)
    false
;;
