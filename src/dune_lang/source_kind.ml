open Stdune
open Dune_sexp

module Host = struct
  type kind =
    | Github
    | Bitbucket
    | Gitlab
    | Sourcehut

  let to_string = function
    | Github -> "github"
    | Bitbucket -> "bitbucket"
    | Gitlab -> "gitlab"
    | Sourcehut -> "sourcehut"
  ;;

  type repo =
    | User_repo of
        { user : string
        ; repo : string
        }
    | Org_repo of
        { org : string
        ; proj : string
        ; repo : string
        }

  type t =
    { repo : repo
    ; kind : kind
    }

  let dyn_of_kind kind = kind |> to_string |> Dyn.string

  let to_dyn { repo; kind } =
    let open Dyn in
    match repo with
    | User_repo { user; repo } ->
      record [ "kind", dyn_of_kind kind; "user", string user; "repo", string repo ]
    | Org_repo { org; proj; repo } ->
      record
        [ "kind", dyn_of_kind kind
        ; "org", string org
        ; "proj", string proj
        ; "repo", string repo
        ]
  ;;

  let host_of_kind = function
    | Github -> "github.com"
    | Bitbucket -> "bitbucket.org"
    | Gitlab -> "gitlab.com"
    | Sourcehut -> "sr.ht"
  ;;

  let base_uri { repo; kind } =
    let host = host_of_kind kind in
    match repo with
    | User_repo { user; repo } ->
      sprintf
        "%s/%s/%s"
        host
        (match kind with
         | Sourcehut -> "~" ^ user
         | _ -> user)
        repo
    | Org_repo { org; proj; repo } -> sprintf "%s/%s/%s/%s" host org proj repo
  ;;

  let add_https s = "https://" ^ s
  let homepage t = add_https (base_uri t)

  let bug_reports t =
    match t.kind with
    | Sourcehut -> add_https ("todo." ^ base_uri t)
    | _ ->
      homepage t
      ^
        (match t.kind with
        | Sourcehut -> assert false
        | Bitbucket | Github -> "/issues"
        | Gitlab -> "/-/issues")
  ;;

  let enum k =
    [ "GitHub", Github, None
    ; "Bitbucket", Bitbucket, Some (2, 8)
    ; "Gitlab", Gitlab, Some (2, 8)
    ; "Sourcehut", Sourcehut, Some (3, 1)
    ]
    |> List.map ~f:(fun (name, kind, since) ->
      let of_string ~loc s =
        match String.split ~on:'/' s with
        | [ user; repo ] -> k { repo = User_repo { user; repo }; kind }
        | [ org; proj; repo ] -> k { repo = Org_repo { org; proj; repo }; kind }
        | _ ->
          User_error.raise
            ~loc
            [ Pp.textf "%s repository must be of form user/repo" name ]
      in
      let decode =
        let open Decoder in
        (match since with
         | None -> return ()
         | Some v -> Syntax.since Stanza.syntax v)
        >>> plain_string of_string
      in
      let constr = to_string kind in
      constr, decode)
  ;;

  let encode { repo; kind } =
    let forge = to_string kind in
    let path =
      match repo with
      | User_repo { user; repo } -> sprintf "%s/%s" user repo
      | Org_repo { org; proj; repo } -> sprintf "%s/%s/%s" org proj repo
    in
    let open Encoder in
    pair string string (forge, path)
  ;;

  let to_string t =
    let base_uri =
      let base = base_uri t in
      match t.kind with
      | Sourcehut -> "git." ^ base
      | _ -> base ^ ".git"
    in
    "git+https://" ^ base_uri
  ;;
end

type t =
  | Host of Host.t
  | Url of string

let to_dyn =
  let open Dyn in
  function
  | Host h -> variant "Host" [ Host.to_dyn h ]
  | Url url -> variant "Url" [ string url ]
;;

let to_string = function
  | Host h -> Host.to_string h
  | Url u -> u
;;

let encode =
  let open Encoder in
  function
  | Url url -> pair string string ("uri", url)
  | Host host -> Host.encode host
;;

let decode =
  let open Decoder in
  sum
    (( "uri"
     , let+ s = string in
       Url s )
     :: Host.enum (fun x -> Host x))
;;
