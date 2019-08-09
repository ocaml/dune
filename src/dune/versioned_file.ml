open! Stdune
open Import

module type S = sig
  type data

  module Lang : sig
    val register : Syntax.t -> data -> unit
    module Instance : sig
      type t =
        { syntax  : Syntax.t
        ; data    : data
        ; version : Syntax.Version.t
        }
    end
    val get_exn : string -> Instance.t
  end
  val load : Path.t -> f:(Lang.Instance.t -> 'a Dune_lang.Decoder.t) -> 'a
  val parse_contents
    :  Lexing.lexbuf
    -> Dune_lexer.first_line
    -> f:(Lang.Instance.t -> 'a Dune_lang.Decoder.t)
    -> 'a
end

module Make(Data : sig type t end) = struct
  module Lang = struct
    type t =
      { syntax : Syntax.t
      ; data   : Data.t
      }

    module Instance = struct
      type t =
        { syntax  : Syntax.t
        ; data    : Data.t
        ; version : Syntax.Version.t
        }
    end

    let langs = Table.create (module String) 32

    let register syntax data =
      let name = Syntax.name syntax in
      if Table.mem langs name then
        Code_error.raise "Versioned_file.Lang.register: already registered"
          [ "name", Dyn.Encoder.string name ];
      Table.add_exn langs name { syntax; data }

    let parse first_line : Instance.t =
      let { Dune_lexer.
            lang    = (name_loc, name)
          ; version = (ver_loc, ver)
          } = first_line
      in
      let ver =
        Dune_lang.Decoder.parse Syntax.Version.decode Univ_map.empty
          (Atom (ver_loc, Dune_lang.Atom.of_string ver)) in
      match Table.find langs name with
      | None ->
        User_error.raise ~loc:name_loc
          [ Pp.textf "Unknown language %S." name ]
          ~hints:(User_message.did_you_mean name
                    ~candidates:(Table.keys langs))
      | Some t ->
        Syntax.check_supported t.syntax (ver_loc, ver);
        { syntax  = t.syntax
        ; data    = t.data
        ; version = ver
        }

    let get_exn name : Instance.t =
      let t = Table.find_exn langs name in
      { syntax  = t.syntax
      ; data    = t.data
      ; version = Syntax.greatest_supported_version t.syntax
      }
  end

  let parse_contents lb first_line ~f =
    let lang = Lang.parse first_line in
    let sexp = Dune_lang.Parser.parse lb ~mode:Many_as_one in
    let parsing_context =
      Univ_map.singleton (Syntax.key lang.syntax) lang.version
    in
    Dune_lang.Decoder.parse (Dune_lang.Decoder.enter (f lang)) parsing_context sexp

  let load fn ~f =
    Io.with_lexbuf_from_file fn ~f:(fun lb ->
      parse_contents lb (Dune_lexer.first_line lb) ~f)
end

let no_more_lang =
  let open Dune_lang.Decoder in
  let+ (_ : _ list) =
    multi_field "lang"
      (let+ loc = loc
       and+ _ = repeat raw
       in
       User_error.raise ~loc
         [ Pp.text "The (lang ..) line cannot appear more than once." ])
  in
  ()
