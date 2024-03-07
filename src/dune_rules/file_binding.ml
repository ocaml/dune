open Import
open Memo.O

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

let relative_path_starts_with_parent relative_path =
  match String.lsplit2 relative_path ~on:'/' with
  | None -> Filename.(equal relative_path parent_dir_name)
  | Some (first, _) -> String.equal first Filename.parent_dir_name
;;

let escaping_paths_in_install_stanza =
  Warning.make
    ~default:(fun version -> if version >= (3, 11) then `Enabled else `Disabled)
    ~name:"escaping_paths_in_install_stanza"
    ~since:(3, 11)
;;

let validate_dst_for_install_stanza
  ~relative_dst_path_starts_with_parent_error_when
  ~loc
  ~dst
  ~dir
  =
  match relative_path_starts_with_parent dst with
  | false -> Memo.return ()
  | true ->
    (match relative_dst_path_starts_with_parent_error_when with
     | `Deprecation_warning_from_3_11 ->
       Warning_emit.emit
         escaping_paths_in_install_stanza
         (Warning_emit.Context.source_dir_or_enable dir)
         (fun () ->
            User_message.make
              ~loc
              [ Pp.textf
                  "The destination path %s begins with %s which will become an error in \
                   a future version of Dune. Destinations of files in install stanzas \
                   beginning with %s will be disallowed to prevent a package's installed \
                   files from escaping that package's install directories."
                  (String.maybe_quoted dst)
                  (String.maybe_quoted Filename.parent_dir_name)
                  (String.maybe_quoted Filename.parent_dir_name)
              ]
            |> Memo.return)
     | `Always_error ->
       User_error.raise
         ~loc
         [ Pp.textf
             "The destination path %s begins with %s which is not allowed. Destinations \
              in install stanzas may not begin with %s to prevent a package's installed \
              files from escaping that package's install directories."
             (String.maybe_quoted dst)
             (String.maybe_quoted Filename.parent_dir_name)
             (String.maybe_quoted Filename.parent_dir_name)
         ])
;;

module Expanded = struct
  type nonrec t = (Loc.t * Path.Build.t, Loc.t * string) t

  let to_dyn =
    let open Dyn in
    to_dyn (pair Loc.to_dyn Path.Build.to_dyn) (pair Loc.to_dyn string)
  ;;

  let src t = snd t.src
  let dst t = Option.map ~f:snd t.dst
  let src_loc t = fst t.src

  let dst_basename { src = _, src; dst; dune_syntax = _; dir = _ } =
    match dst with
    | Some (_, dst) -> dst
    | None ->
      let basename = Path.Build.basename src in
      String.drop_suffix basename ~suffix:".exe" |> Option.value ~default:basename
  ;;

  let dst_path t ~dir = Path.Build.relative dir (dst_basename t)

  let validate_for_install_stanza t ~relative_dst_path_starts_with_parent_error_when =
    Memo.Option.iter t.dst ~f:(fun (loc, dst) ->
      validate_dst_for_install_stanza
        ~relative_dst_path_starts_with_parent_error_when
        ~loc
        ~dst
        ~dir:t.dir)
  ;;
end

module Unexpanded = struct
  type nonrec t = (String_with_vars.t, String_with_vars.t) t

  let loc t = String_with_vars.loc t.src
  let to_dyn = to_dyn String_with_vars.to_dyn String_with_vars.to_dyn
  let equal = equal String_with_vars.equal_no_loc String_with_vars.equal_no_loc

  let make ~src:(locs, src) ~dst:(locd, dst) ~dune_syntax ~dir =
    { src = String_with_vars.make_text locs src
    ; dst = Some (String_with_vars.make_text locd dst)
    ; dune_syntax
    ; dir
    }
  ;;

  let expand_src t ~dir ~f = f t.src >>| Path.Build.relative dir

  let destination_relative_to_install_path t ~section ~expand ~expand_partial =
    let* src = expand_partial t.src
    and+ dst_loc_opt =
      Memo.Option.map t.dst ~f:(fun dst ->
        let loc = String_with_vars.loc dst in
        let+ dst = expand dst in
        dst, loc)
    in
    let+ () =
      Memo.Option.iter dst_loc_opt ~f:(fun (dst, loc) ->
        validate_dst_for_install_stanza
          ~relative_dst_path_starts_with_parent_error_when:`Deprecation_warning_from_3_11
          ~loc
          ~dst
          ~dir:t.dir)
    in
    Install.Entry.adjust_dst ~section ~src ~dst:(Option.map dst_loc_opt ~f:fst)
  ;;

  let expand t ~dir ~f =
    let f sw =
      let+ f = f sw in
      String_with_vars.loc sw, f
    in
    let* src =
      let+ loc, expanded = f t.src in
      loc, Path.Build.relative dir expanded
    in
    let+ dst =
      match t.dst with
      | None -> Memo.return None
      | Some dst ->
        let+ loc, p = f dst in
        Some (loc, p)
    in
    { src
    ; dst
    ; dune_syntax = t.dune_syntax
    ; dir = Some (Path.Build.drop_build_context_exn dir)
    }
  ;;

  let decode =
    let open Dune_lang.Decoder in
    let decode =
      let+ is_atom =
        peek_exn
        >>| function
        | Atom _ -> true
        | _ -> false
      and+ s = String_with_vars.decode
      and+ dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
      if (not is_atom) && dune_syntax < (1, 6)
      then (
        let what =
          (if String_with_vars.has_pforms s then "variables" else "quoted strings")
          |> sprintf "Using %s here"
        in
        Dune_lang.Syntax.Error.since (String_with_vars.loc s) Stanza.syntax (1, 6) ~what)
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
        ~loc:(Dune_lang.Ast.loc sexp)
        [ Pp.text "Invalid format, <name> or (<name> as <install-as>) expected" ]
  ;;

  let dune_syntax t = t.dune_syntax

  module L = struct
    let decode = Dune_lang.Decoder.repeat decode

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
