(* TODO already exists in stdune/bin.ml *)
let path_sep = if Sys.win32 then ';' else ':'
let dune_dir_locations_env_var = "DUNE_DIR_LOCATIONS"
let dune_ocaml_stdlib_env_var = "DUNE_OCAML_STDLIB"
let dune_ocaml_hardcoded_env_var = "DUNE_OCAML_HARDCODED"
let dune_sourceroot_env_var = "DUNE_SOURCEROOT"

type entry =
  { package : string
  ; section : Dune_section.t
  ; dir : string
  }

let decode_dune_dir_locations =
  let rec aux acc = function
    | [] -> Some (List.rev acc)
    | package :: section :: dir :: l ->
      let section =
        match Dune_section.of_string section with
        | None -> invalid_arg ("Dune-site: Unknown section " ^ section)
        | Some s -> s
      in
      aux ({ package; section; dir } :: acc) l
    | _ -> None
  in
  fun s ->
    let l = String.split_on_char path_sep s in
    aux [] l
;;

let encode_dune_dir_locations =
  let add b { package; section; dir } =
    Buffer.add_string b package;
    Buffer.add_char b path_sep;
    Buffer.add_string b (Dune_section.to_string section);
    Buffer.add_char b path_sep;
    Buffer.add_string b dir
  in
  let rec loop b = function
    | [] -> ()
    | [ x ] -> add b x
    | x :: xs ->
      add b x;
      Buffer.add_char b path_sep;
      loop b xs
  in
  fun s ->
    let b = Buffer.create 16 in
    loop b s;
    Buffer.contents b
;;
