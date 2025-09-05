open Import

type ('src, 'dst) t =
  { src : 'src
  ; dst : 'dst option
    (* The [dune_syntax] field is used for validation which has different
         behaviour depending on the version of dune syntax in use. *)
  ; dune_syntax : Syntax.Version.t
  ; dir : Path.Source.t option
  }

let to_dyn f g { src; dst; dune_syntax; dir } =
  let open Dyn in
  record
    [ "src", f src
    ; "dst", option g dst
    ; "dune_syntax", Syntax.Version.to_dyn dune_syntax
    ; "dir", option Path.Source.to_dyn dir
    ]
;;

let equal f g { src; dst; dune_syntax; dir } t =
  f src t.src
  && Option.equal g dst t.dst
  && Syntax.Version.equal dune_syntax t.dune_syntax
  && Option.equal Path.Source.equal dir t.dir
;;

module Expanded = struct
  type nonrec t = (Loc.t * Path.Build.t, Loc.t * string) t

  let to_dyn =
    let open Dyn in
    to_dyn (pair Loc.to_dyn Path.Build.to_dyn) (pair Loc.to_dyn string)
  ;;

  let src t = snd t.src
  let dst t = Option.map ~f:snd t.dst
  let dst_with_loc t = t.dst
  let dir (t : t) = t.dir
  let src_loc t = fst t.src

  let dst_basename { src = _, src; dst; dune_syntax = _; dir = _ } =
    match dst with
    | Some (_, dst) -> dst
    | None ->
      let basename = Path.Build.basename src in
      String.drop_suffix basename ~suffix:".exe" |> Option.value ~default:basename
  ;;

  let dst_path t ~dir = Path.Build.relative dir (dst_basename t)
end

module Unexpanded = struct
  type nonrec t = (String_with_vars.t, String_with_vars.t) t

  let src t = t.src
  let loc t = String_with_vars.loc t.src
  let to_dyn = to_dyn String_with_vars.to_dyn String_with_vars.to_dyn
  let equal = equal String_with_vars.equal_no_loc String_with_vars.equal_no_loc
  let dst (t : t) = t.dst
  let dir t = t.dir

  let make ~src:(locs, src) ~dst:(locd, dst) ~dune_syntax ~dir =
    { src = String_with_vars.make_text locs src
    ; dst = Some (String_with_vars.make_text locd dst)
    ; dune_syntax
    ; dir
    }
  ;;

  let expand t ~dir ~src ~dst = { t with src; dst; dir = Some dir }

  let decode =
    let open Decoder in
    let decode =
      let+ is_atom =
        peek_exn
        >>| function
        | Atom _ -> true
        | _ -> false
      and+ s = String_with_vars.decode
      and+ dune_syntax = Syntax.get_exn Stanza.syntax in
      if (not is_atom) && dune_syntax < (1, 6)
      then (
        let what =
          (if String_with_vars.has_pforms s then "variables" else "quoted strings")
          |> sprintf "Using %s here"
        in
        Syntax.Error.since (String_with_vars.loc s) Stanza.syntax (1, 6) ~what)
      else s, dune_syntax
    in
    let dir = Dune_project.get () >>| Option.map ~f:Dune_project.root in
    peek_exn
    >>= function
    | Atom _ | Quoted_string _ | Template _ ->
      let+ src, dune_syntax = decode
      and+ dir = dir in
      { src; dst = None; dune_syntax; dir }
    | List (_, [ _; Atom (_, A "as"); _ ]) ->
      enter
        (let* src, dune_syntax = decode in
         keyword "as"
         >>> let* dst, _ = decode
             and+ dir = dir in
             return { src; dst = Some dst; dune_syntax; dir })
    | sexp ->
      User_error.raise
        ~loc:(Ast.loc sexp)
        [ Pp.text "Invalid format, <name> or (<name> as <install-as>) expected" ]
  ;;

  let dune_syntax t = t.dune_syntax

  module L = struct
    let decode = Decoder.repeat decode

    let strings_with_vars { src; dst; dune_syntax = _; dir = _ } =
      src :: Option.to_list dst
    ;;

    let find_pform fbs =
      List.find_map fbs ~f:(fun fb ->
        List.find_map (strings_with_vars fb) ~f:(fun sw ->
          match String_with_vars.text_only sw with
          | None -> Some (String_with_vars.loc sw)
          | Some _ -> None))
    ;;
  end
end
