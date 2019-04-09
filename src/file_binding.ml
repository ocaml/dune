open! Stdune

type 'a t =
  { src : 'a
  ; dst : 'a option
  }

module Expanded = struct
  type nonrec t = (Loc.t * string) t

  let src t = snd t.src
  let dst t = Option.map ~f:snd t.dst

  let src_loc t = fst t.src

  let src_path { src = (_, src); _ } ~dir = Path.relative dir src
  let dst_basename { src = (_, src); dst } =
    match dst with
    | Some (_, dst) -> dst
    | None ->
      let basename = Filename.basename src in
      String.drop_suffix basename ~suffix:".exe"
      |> Option.value ~default:basename

  let dst_path t ~dir =
    Path.relative dir (dst_basename t)
end

module Unexpanded = struct
  type nonrec t = String_with_vars.t t

  let make ~src:(locs, src) ~dst:(locd, dst) =
    { src = String_with_vars.make_text locs src
    ; dst = Some (String_with_vars.make_text locd dst)
    }

  let expand_src t ~f = f t.src

  let expand t ~f =
    let f sw = (String_with_vars.loc sw, f sw) in
    { src = f t.src
    ; dst = Option.map ~f t.dst
    }

  module L = struct

    let decode_file =
      let open Stanza.Decoder in
      let decode =
        let+ is_atom =
          peek_exn >>| function
          | Atom _ -> true
          | _ -> false
        and+ s = String_with_vars.decode
        and+ version = Syntax.get_exn Stanza.syntax in
        if not is_atom && version < (1, 6) then
          let what =
            (if String_with_vars.has_vars s then
               "variables"
             else
               "quoted strings")
            |> sprintf "Using %s here"
          in
          Syntax.Error.since (String_with_vars.loc s) Stanza.syntax (1, 6) ~what;
        else
          s
      in
      peek_exn >>= function
      | Atom _ | Quoted_string _ | Template _ ->
        decode >>| fun src ->
        { src; dst = None }
      | List (_, [_; Atom (_, A "as"); _]) ->
        enter
          (let* src = decode in
           keyword "as" >>>
           let* dst = decode in
           return { src; dst = Some dst })
      | sexp ->
        of_sexp_error (Dune_lang.Ast.loc sexp)
          "invalid format, <name> or (<name> as <install-as>) expected"

    let decode =
      let open Stanza.Decoder in list decode_file
  end
end
