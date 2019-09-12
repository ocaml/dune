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

  val load : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a

  val parse : Lexing.lexbuf -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a

  val parse_contents :
    Lexing.lexbuf -> First_line.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
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

    let langs = Table.create (module String) 32

    let register syntax data =
      let name = Syntax.name syntax in
      if Table.mem langs name then
        Code_error.raise "Versioned_file.Lang.register: already registered"
          [ ("name", Dyn.Encoder.string name) ];
      Table.add_exn langs name { syntax; data }

    let parse first_line : Instance.t =
      let { First_line.lang = name_loc, name; version = ver_loc, ver } =
        first_line
      in
      let ver =
        Decoder.parse Syntax.Version.decode Univ_map.empty
          (Atom (ver_loc, Atom.of_string ver))
      in
      match Table.find langs name with
      | None ->
        User_error.raise ~loc:name_loc
          [ Pp.textf "Unknown language %S." name ]
          ~hints:
            (User_message.did_you_mean name ~candidates:(Table.keys langs))
      | Some t ->
        Syntax.check_supported t.syntax (ver_loc, ver);
        { syntax = t.syntax; data = t.data; version = ver }

    let get_exn name : Instance.t =
      let t = Table.find_exn langs name in
      { syntax = t.syntax
      ; data = t.data
      ; version = Syntax.greatest_supported_version t.syntax
      }
  end

  let parse_contents lb first_line ~f =
    let lang = Lang.parse first_line in
    let sexp = Parser.parse lb ~mode:Many_as_one in
    let parsing_context =
      Univ_map.singleton (Syntax.key lang.syntax) lang.version
    in
    Decoder.parse (Decoder.enter (f lang)) parsing_context sexp

  let parse lb ~f = parse_contents lb (First_line.lex lb) ~f

  let load fn ~f = Io.with_lexbuf_from_file fn ~f:(parse ~f)
end

let no_more_lang =
  let open Decoder in
  let+ (_ : _ list) =
    multi_field "lang"
      (let+ loc = loc
       and+ _ = repeat raw in
       User_error.raise ~loc
         [ Pp.text "The (lang ..) line cannot appear more than once." ])
  in
  ()
