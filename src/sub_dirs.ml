open! Stdune

type 'set t =
  { sub_dirs : 'set
  ; data_only : 'set
  }

let default =
  let standard_dirs =
    Predicate_lang.of_pred (function
      | "" -> false
      | s -> s.[0] <> '.' && s.[0] <> '_')
  in
  { sub_dirs = standard_dirs
  ; data_only = Predicate_lang.empty
  }

let make ~sub_dirs ~ignored_sub_dirs ~data_only =
  let sub_dirs =
    let ignored_sub_dirs = Predicate_lang.union ignored_sub_dirs in
    let sub_dirs = Option.value sub_dirs ~default:default.sub_dirs in
    Predicate_lang.diff sub_dirs ignored_sub_dirs
  in
  let data_only = Option.value data_only ~default:default.data_only in
  { sub_dirs ; data_only }

let add_data_only_dirs t ~dirs =
  { t with data_only = Predicate_lang.union [t.data_only; dirs] }

let eval t ~dirs =
  let sub_dirs =
    Predicate_lang.filter t.sub_dirs ~standard:default.sub_dirs dirs
  in
  let data_only =
    Predicate_lang.filter t.data_only ~standard:default.data_only sub_dirs
    |> String.Set.of_list
  in
  let sub_dirs = String.Set.of_list sub_dirs in
  { sub_dirs
  ; data_only
  }

module Status = struct
  type t = Ignored | Data_only | Normal
end

let status t ~dir =
  match String.Set.mem t.sub_dirs dir
      , String.Set.mem t.data_only dir
  with
  | true, false  -> Status.Normal
  | true, true   -> Data_only
  | false, false -> Ignored
  | false, true  -> assert false

module Stanza = struct
  type t =
    | Ignore_sub_dirs of Predicate_lang.t
    | Sub_dirs of Predicate_lang.t
    | Data_only of Predicate_lang.t

  let decode : t Dune_lang.Decoder.t =
    let ignore_sub_dirs =
      let open Dune_lang.Decoder in
      plain_string (fun ~loc dn ->
        if Filename.dirname dn <> Filename.current_dir_name ||
           match dn with
           | "" | "." | ".." -> true
           | _ -> false
        then
          of_sexp_errorf loc "Invalid sub-directory name %S" dn
        else
          dn)
      |> list
      >>| (fun l ->
        Ignore_sub_dirs (Predicate_lang.of_string_set (String.Set.of_list l)))
    in
    let open Dune_lang.Decoder in
    let sub_dirs =
      Syntax.since Stanza.syntax (1, 6) >>>
      Predicate_lang.decode >>| fun plang ->
      Sub_dirs plang
    in
    let data_only =
      Syntax.since Stanza.syntax (1, 6) >>>
      Predicate_lang.decode >>| fun plang ->
      Data_only plang
    in
    sum [ "ignored_subdirs", ignore_sub_dirs
        ; "subdirs", sub_dirs
        ; "data_only_dirs", data_only
        ]

  let extract ~project sexps =
    let (sub_dirs, ignored_sub_dirs, data_only), sexps =
      List.fold_left sexps ~init:((None, [], None), [])
        ~f:(fun ((sub_dirs, ignored, data_only) as stanzas, sexps) sexp ->
          match (sexp : Dune_lang.Ast.t) with
          | List ( loc ,
                   (Atom (_ , A ("ignored_subdirs"
                                | "subdirs" | "data_only_dirs")) :: _)) ->
            let stanza =
              Dune_project.set_parsing_context project decode in
            let stanza = Dune_lang.Decoder.parse stanza Univ_map.empty sexp in
            let stanzas =
              match stanza, sub_dirs, data_only, ignored with
              | Ignore_sub_dirs i, None, _, _ ->
                (sub_dirs, i :: ignored, data_only)
              | Data_only x, _, None, _ -> (sub_dirs, ignored, Some x)
              | Sub_dirs x, None, _, [] -> (Some x, [], data_only)
              | Sub_dirs _, Some _, _, _ ->
                Errors.fail loc "More than one sub_dirs stanza is not allowed"
              | Ignore_sub_dirs _, Some _, _, _
              | Sub_dirs _, _, _, _::_ ->
                Errors.fail loc
                  "Cannot have both sub_dirs and ignored_sub_dirs \
                   stanza in a dune file. "
              | Data_only _, _, Some _, _ ->
                Errors.fail loc "More than one data_only stanza is not allowed"
            in
            (stanzas, sexps)
          | _ -> stanzas, sexp :: sexps)
    in
    let sexps = List.rev sexps in
    (make ~sub_dirs ~data_only ~ignored_sub_dirs, sexps)
end
