open! Stdune
module First_line = Versioned_file_first_line

module type S = sig
  type data

  module Lang : sig
    val register : Syntax.t -> data -> unit

    module Instance : sig
      type t =
        { syntax : Syntax.t
        ; data : data
        ; version : Syntax.Version.t
        }
    end

    val get_exn : string -> Instance.t
  end

  val load_exn : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
  val load : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a Or_exn.t
  val parse_contents : Lexing.lexbuf -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
end

module Make (Data : sig
    type t
  end) =
struct
  module Lang = struct
    type t =
      { syntax : Syntax.t
      ; data : Data.t
      }

    module Instance = struct
      type t =
        { syntax : Syntax.t
        ; data : Data.t
        ; version : Syntax.Version.t
        }
    end

    (* This mutable table is safe under the assumption that we call [register]
       only at the top level, which is currently true. *)
    let langs = Table.create (module String) 32

    let register syntax data =
      let name = Syntax.name syntax in
      if Table.mem langs name
      then
        Code_error.raise
          "Versioned_file.Lang.register: already registered"
          [ "name", Dyn.string name ];
      Table.add_exn langs name { syntax; data }
    ;;

    let parse first_line : Instance.t =
      let { First_line.lang = name_loc, name; version = ver_loc, ver } = first_line in
      let dune_lang_ver =
        Decoder.parse
          Syntax.Version.decode
          Univ_map.empty
          (Atom (ver_loc, Atom.of_string ver))
      in
      match Table.find langs name with
      | None ->
        User_error.raise
          ~loc:name_loc
          [ Pp.textf "Unknown language %S." name ]
          ~hints:(User_message.did_you_mean name ~candidates:(Table.keys langs))
      | Some t ->
        Syntax.check_supported ~dune_lang_ver t.syntax (ver_loc, dune_lang_ver);
        { syntax = t.syntax; data = t.data; version = dune_lang_ver }
    ;;

    (* TODO get_exn is only called with "dune" so far, but
       greatest_supported_version may return None for extensions which are not
       supported under the specified dune_lang version *)
    let get_exn name : Instance.t =
      let t = Table.find_exn langs name in
      { syntax = t.syntax
      ; data = t.data
      ; version = Syntax.greatest_supported_version_exn t.syntax
      }
    ;;
  end

  let parse_lang_exn lb =
    let first_line = First_line.lex lb in
    let lang = Lang.parse first_line in
    lang, Parser.parse lb ~mode:Many_as_one
  ;;

  let parse_ast ((lang : Lang.Instance.t), ast) ~f =
    let parsing_context =
      Univ_map.singleton (Syntax.key lang.syntax) (Active lang.version)
    in
    Decoder.parse (Decoder.enter (f lang)) parsing_context ast
  ;;

  let parse_contents lb ~f =
    let ast = parse_lang_exn lb in
    parse_ast ast ~f
  ;;

  let load fn ~f =
    Io.with_lexbuf_from_file fn ~f:(fun lb ->
      Result.try_with (fun () -> parse_contents lb ~f))
  ;;

  let load_exn fn ~f = Result.ok_exn (load fn ~f)
end

let no_more_lang =
  let open Decoder in
  let+ (_ : _ list) =
    multi_field
      "lang"
      (let+ loc = loc
       and+ _ = repeat raw in
       User_error.raise
         ~loc
         [ Pp.text "The (lang ..) line cannot appear more than once." ])
  in
  ()
;;
