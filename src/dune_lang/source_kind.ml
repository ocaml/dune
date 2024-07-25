open Stdune
open Dune_sexp

module Host = struct
  type user_repo =
    { user : string
    ; repo : string
    }

  type gitlab_repo =
    | User_repo of user_repo
    | Org_repo of
        { org : string
        ; proj : string
        ; repo : string
        }

  type t =
    | Github of user_repo
    | Bitbucket of user_repo
    | Gitlab of gitlab_repo
    | Sourcehut of user_repo

  let kind_string = function
    | Github _ -> "github"
    | Bitbucket _ -> "bitbucket"
    | Gitlab _ -> "gitlab"
    | Sourcehut _ -> "sourcehut"
  ;;

  let dyn_of_user_repo kind { user; repo } =
    let open Dyn in
    record [ "kind", kind; "user", string user; "repo", string repo ]
  ;;

  let dyn_of_gitlab_repo kind repo =
    match repo with
    | User_repo user_repo -> dyn_of_user_repo kind user_repo
    | Org_repo { org; proj; repo } ->
      let open Dyn in
      record [ "kind", kind; "org", string org; "proj", string proj; "repo", string repo ]
  ;;

  let to_dyn repo =
    let kind = Dyn.string (kind_string repo) in
    match repo with
    | Gitlab gitlab_repo -> dyn_of_gitlab_repo kind gitlab_repo
    | Github user_repo | Bitbucket user_repo | Sourcehut user_repo ->
        dyn_of_user_repo kind user_repo
  ;;

  let host_of_repo = function
    | Github _ -> "github.com"
    | Bitbucket _ -> "bitbucket.org"
    | Gitlab _ -> "gitlab.com"
    | Sourcehut _ -> "sr.ht"
  ;;

  let base_uri repo =
    let host = host_of_repo repo in
    match repo with
    | Gitlab (Org_repo {org; proj; repo}) -> sprintf "%s/%s/%s/%s" host org proj repo
    | Sourcehut {user; repo} -> sprintf "%s/~%s/%s" host user repo
    | Gitlab (User_repo {user; repo}) | Github {user; repo} | Bitbucket {user; repo} ->
        sprintf "%s/%s/%s" host user repo
  ;;

  let add_https s = "https://" ^ s
  let homepage t = add_https (base_uri t)

  let bug_reports = function
    | Gitlab _ as repo -> homepage repo ^ "/-/issues"
    | Github _ as repo -> homepage repo ^ "/issues"
    | Bitbucket _ as repo -> homepage repo ^ "/issues"
    | Sourcehut _ as repo -> add_https ("todo." ^ base_uri repo)
  ;;

  (* todo -- @H-ANSEN
     currently each forge in the list is evaluated in order, since 'gitlab' 
     now has the option to specifiy a organization style repo we need some way
     to identify this type and present a error in the case that the dune version
     is not supported. Currently no error is thrown *)
  let enum k =
    let stub_user_repo = { user = ""; repo = "" } in
    let stub_org_repo = Org_repo { org = ""; proj = ""; repo = "" } in
    [ "Github", Github stub_user_repo, None
    ; "Bitbucket", Bitbucket stub_user_repo, Some (2, 8)
    ; "Sourcehut", Sourcehut stub_user_repo, Some (3, 1)
    ; "Gitlab", Gitlab (User_repo stub_user_repo), Some (2, 8)
    ; "Gitlab", Gitlab stub_org_repo, Some (3, 17)
    ]
    |> List.map ~f:(fun (name, kind, since) ->
      let of_string ~loc str =
        match kind, String.split ~on:'/' str with
        | Gitlab _, [ user; repo ] -> k @@ Gitlab (User_repo { user; repo })
        | Gitlab _, [ org; proj; repo ] -> k @@ Gitlab (Org_repo { org; proj; repo })
        | Github _, [ user; repo ] -> k @@ Github { user; repo }
        | Bitbucket _, [ user; repo ] -> k @@ Bitbucket { user; repo }
        | Sourcehut _, [ user; repo ] -> k @@ Sourcehut { user; repo }
        | _, [ _; _; _ ] ->
          User_error.raise
            ~loc
            ~hints:
              [ Pp.textf
                  "The provided form 'org/proj/repo' is specific to Gitlab projects"
              ]
            [ Pp.textf "%s repository must be of form user/repo" name ]
        | _, _ ->
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
      kind_string kind, decode)
  ;;

  let encode repo =
    let path =
      match repo with
      | Gitlab (Org_repo { org; proj; repo }) -> sprintf "%s/%s/%s" org proj repo
      | Gitlab (User_repo { user; repo }) -> sprintf "%s/%s" user repo
      | Sourcehut { user; repo } -> sprintf "%s/%s" user repo
      | Github { user; repo } -> sprintf "%s/%s" user repo
      | Bitbucket { user; repo } -> sprintf "%s/%s" user repo
    in
    let open Encoder in
    let forge = kind_string repo in
    pair string string (forge, path)
  ;;

  let to_string repo =
    let base_uri =
      let base = base_uri repo in
      match repo with
      | Sourcehut _ -> "git." ^ base
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
