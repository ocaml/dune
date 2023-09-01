open Stdune
open Dune_sexp
open Decoder

module Glob_files = struct
  type t =
    { glob : String_with_vars.t
    ; recursive : bool
    }

  let to_dyn { glob; recursive } =
    Dyn.record [ "glob", String_with_vars.to_dyn glob; "recursive", Dyn.bool recursive ]
  ;;
end

module Sandbox_config = struct
  type t = Loc.t * [ `None | `Always | `Preserve_file_kind ] list

  let all =
    [ "none", `None; "always", `Always; "preserve_file_kind", `Preserve_file_kind ]
  ;;

  let loc (loc, _) = loc

  let string_of_mode mode =
    List.find_map all ~f:(fun (s, mode') ->
      if Poly.equal mode mode' then Some s else None)
    |> Option.value_exn
  ;;

  let encode (_, list) =
    Dune_sexp.List (List.map list ~f:(fun x -> Dune_sexp.atom @@ string_of_mode x))
  ;;

  let decode : t Decoder.t =
    Syntax.since Stanza.syntax (1, 12) >>> located (repeat (enum all))
  ;;

  let fold (_, xs) ~f ~init = List.fold_left xs ~init ~f:(fun acc a -> f a acc)
end

type t =
  | File of String_with_vars.t
  | Alias of String_with_vars.t
  | Alias_rec of String_with_vars.t
  | Glob_files of Glob_files.t
  | Source_tree of String_with_vars.t
  | Package of String_with_vars.t
  | Universe
  | Env_var of String_with_vars.t
  | Sandbox_config of Sandbox_config.t
  | Include of string

let remove_locs = function
  | File sw -> File (String_with_vars.remove_locs sw)
  | Alias sw -> Alias (String_with_vars.remove_locs sw)
  | Alias_rec sw -> Alias_rec (String_with_vars.remove_locs sw)
  | Glob_files g -> Glob_files { g with glob = String_with_vars.remove_locs g.glob }
  | Source_tree sw -> Source_tree (String_with_vars.remove_locs sw)
  | Package sw -> Package (String_with_vars.remove_locs sw)
  | Universe -> Universe
  | Env_var sw -> Env_var sw
  | Sandbox_config s -> Sandbox_config s
  | Include s -> Include s
;;

let decode files =
  let files_only =
    match files with
    | `Allow -> return ()
    | `Forbid ->
      let* loc = loc in
      User_error.raise ~loc [ Pp.text "only files are allowed in this position" ]
  in
  let decode =
    let sw = String_with_vars.decode in
    sum
      ~force_parens:true
      [ ("file", sw >>| fun x -> File x)
      ; ("alias", files_only >>> sw >>| fun x -> Alias x)
      ; ("alias_rec", files_only >>> sw >>| fun x -> Alias_rec x)
      ; ( "glob_files"
        , sw >>| fun glob -> Glob_files { Glob_files.glob; recursive = false } )
      ; ( "glob_files_rec"
        , let+ () = Syntax.since Stanza.syntax (3, 0)
          and+ glob = sw in
          Glob_files { Glob_files.glob; recursive = true } )
      ; ("package", files_only >>> sw >>| fun x -> Package x)
      ; "universe", files_only >>> return Universe
      ; ( "files_recursively_in"
        , let+ () = Syntax.renamed_in Stanza.syntax (1, 0) ~to_:"source_tree"
          and+ x = sw in
          Source_tree x )
      ; ( "source_tree"
        , let+ () = Syntax.since Stanza.syntax (1, 0)
          and+ x = sw in
          Source_tree x )
      ; ("env_var", files_only >>> sw >>| fun x -> Env_var x)
      ; ( "sandbox"
        , let+ config = files_only >>> Sandbox_config.decode in
          Sandbox_config config )
      ; ( "include"
        , let+ () = Syntax.since Stanza.syntax (3, 1)
          and+ filename = filename in
          Include filename )
      ]
  in
  decode
  <|> let+ x = String_with_vars.decode in
      File x
;;

let decode_no_files = decode `Forbid
let decode = decode `Allow

open Dune_sexp

let encode = function
  | File t -> List [ Dune_sexp.atom "file"; String_with_vars.encode t ]
  | Alias t -> List [ Dune_sexp.atom "alias"; String_with_vars.encode t ]
  | Alias_rec t -> List [ Dune_sexp.atom "alias_rec"; String_with_vars.encode t ]
  | Glob_files { glob = t; recursive } ->
    List
      [ Dune_sexp.atom (if recursive then "glob_files_rec" else "glob_files")
      ; String_with_vars.encode t
      ]
  | Source_tree t -> List [ Dune_sexp.atom "source_tree"; String_with_vars.encode t ]
  | Package t -> List [ Dune_sexp.atom "package"; String_with_vars.encode t ]
  | Universe -> Dune_sexp.atom "universe"
  | Env_var t -> List [ Dune_sexp.atom "env_var"; String_with_vars.encode t ]
  | Sandbox_config t -> Sandbox_config.encode t
  | Include t -> List [ Dune_sexp.atom "include"; Dune_sexp.atom t ]
;;

let to_dyn t = Dune_sexp.to_dyn (encode t)
