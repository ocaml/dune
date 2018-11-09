open! Stdune

type 'a file =
  { src : 'a
  ; dst : 'a option
  }

type 'a t = 'a file list

let map t ~f =
  List.map t ~f:(fun { src; dst } -> { src = f src; dst = Option.map ~f dst })

let empty = []

module Unexpanded = struct
  type nonrec t = String_with_vars.t t

  let decode_file =
    let open Stanza.Decoder in
    let decode =
      let%map is_atom =
        peek_exn >>| function
        | Atom _ -> true
        | _ -> false
      and s = String_with_vars.decode
      and version = Syntax.get_exn Stanza.syntax in
      if not is_atom && version < (1, 6) then
        let what =
          (if String_with_vars.has_vars s then "variables" else "quoted strings")
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
        (decode >>= fun src ->
         keyword "as" >>>
         decode >>= fun dst ->
         return { src; dst = Some dst })
    | sexp ->
      of_sexp_error (Dune_lang.Ast.loc sexp)
        "invalid format, <name> or (<name> as <install-as>) expected"

  let decode =
    let open Stanza.Decoder in list decode_file
end
