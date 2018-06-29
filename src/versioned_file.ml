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
  val load : Path.t -> f:(Lang.Instance.t -> 'a Sexp.Of_sexp.t) -> 'a
  val parse_contents
    :  Lexing.lexbuf
    -> Dune_lexer.first_line
    -> f:(Lang.Instance.t -> 'a Sexp.Of_sexp.t)
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

    let langs = Hashtbl.create 32

    let register syntax data =
      let name = Syntax.name syntax in
      if Hashtbl.mem langs name then
        Exn.code_error "Versioned_file.Lang.register: already registered"
          [ "name", Sexp.To_sexp.string name ];
      Hashtbl.add langs name { syntax; data }

    let parse first_line : Instance.t =
      let { Dune_lexer.
            lang    = (name_loc, name)
          ; version = (ver_loc, ver)
          } = first_line
      in
      let ver =
        Sexp.Of_sexp.parse Syntax.Version.t Univ_map.empty
          (Atom (ver_loc, Sexp.Atom.of_string ver)) in
      match Hashtbl.find langs name with
      | None ->
        Loc.fail name_loc "Unknown language %S.%s" name
          (hint name (Hashtbl.keys langs))
      | Some t ->
        Syntax.check_supported t.syntax (ver_loc, ver);
        { syntax  = t.syntax
        ; data    = t.data
        ; version = ver
        }

    let get_exn name : Instance.t =
      let t = Option.value_exn (Hashtbl.find langs name) in
      { syntax  = t.syntax
      ; data    = t.data
      ; version = Syntax.greatest_supported_version t.syntax
      }
  end

  let parse_contents lb first_line ~f =
    let lang = Lang.parse first_line in
    let sexp = Sexp.Parser.parse lb ~mode:Many_as_one in
    let parsing_context =
      Univ_map.singleton (Syntax.key lang.syntax) lang.version
    in
    Sexp.Of_sexp.parse (Sexp.Of_sexp.enter (f lang)) parsing_context sexp

  let load fn ~f =
    Io.with_lexbuf_from_file fn ~f:(fun lb ->
      parse_contents lb (Dune_lexer.first_line lb) ~f)
end
