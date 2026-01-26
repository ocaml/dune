open Import

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
    | Codeberg of user_repo
    | Tangled of user_repo

  let kind_string = function
    | Github _ -> "github"
    | Bitbucket _ -> "bitbucket"
    | Gitlab _ -> "gitlab"
    | Sourcehut _ -> "sourcehut"
    | Codeberg _ -> "codeberg"
    | Tangled _ -> "tangled"
  ;;

  let kind_string_noun_case = function
    | Github _ -> "GitHub"
    | Bitbucket _ -> "Bitbucket"
    | Gitlab _ -> "GitLab"
    | Sourcehut _ -> "SourceHut"
    | Codeberg _ -> "Codeberg"
    | Tangled _ -> "Tangled"
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
    | Github user_repo
    | Bitbucket user_repo
    | Sourcehut user_repo
    | Codeberg user_repo
    | Tangled user_repo -> dyn_of_user_repo kind user_repo
  ;;

  let host_of_repo = function
    | Github _ -> "github.com"
    | Bitbucket _ -> "bitbucket.org"
    | Gitlab _ -> "gitlab.com"
    | Sourcehut _ -> "sr.ht"
    | Codeberg _ -> "codeberg.org"
    | Tangled _ -> "tangled.org"
  ;;

  let base_uri repo =
    let host = host_of_repo repo in
    match repo with
    | Gitlab (Org_repo { org; proj; repo }) -> sprintf "%s/%s/%s/%s" host org proj repo
    | Sourcehut { user; repo } -> sprintf "%s/~%s/%s" host user repo
    | Github { user; repo }
    | Bitbucket { user; repo }
    | Tangled { user; repo }
    | Gitlab (User_repo { user; repo })
    | Codeberg { user; repo } -> sprintf "%s/%s/%s" host user repo
  ;;

  let add_https s = "https://" ^ s
  let homepage t = add_https (base_uri t)

  let bug_reports = function
    | Github _ as repo -> homepage repo ^ "/issues"
    | Bitbucket _ as repo -> homepage repo ^ "/issues"
    | Gitlab _ as repo -> homepage repo ^ "/-/issues"
    | Sourcehut _ as repo -> add_https ("todo." ^ base_uri repo)
    | Codeberg _ as repo -> homepage repo ^ "/issues"
    | Tangled _ as repo -> homepage repo ^ "/issues"
  ;;

  let enum k =
    let stub_user_repo = { user = ""; repo = "" } in
    let stub_org_repo = Org_repo { org = ""; proj = ""; repo = "" } in
    let repo_name k = k |> kind_string |> String.capitalize in
    [ Github stub_user_repo
    ; Bitbucket stub_user_repo
    ; Sourcehut stub_user_repo
    ; Codeberg stub_user_repo
    ; Tangled stub_user_repo
    ; Gitlab (User_repo stub_user_repo)
    ; Gitlab stub_org_repo
    ]
    |> List.map ~f:(fun kind ->
      let of_string ~loc str =
        let name = repo_name kind
        and name_cased = kind_string_noun_case kind in
        match kind, String.split ~on:'/' str with
        | Github _, [ user; repo ] -> Github { user; repo }, None
        | Bitbucket _, [ user; repo ] -> Bitbucket { user; repo }, Some ((2, 8), name)
        | Sourcehut _, [ user; repo ] -> Sourcehut { user; repo }, Some ((3, 1), name)
        | Codeberg _, [ user; repo ] -> Codeberg { user; repo }, Some ((3, 17), name)
        | Tangled _, [ user; repo ] -> Tangled { user; repo }, Some ((3, 21), name)
        | Gitlab _, [ user; repo ] ->
          Gitlab (User_repo { user; repo }), Some ((2, 8), name)
        | Gitlab _, [ org; proj; repo ] ->
          ( Gitlab (Org_repo { org; proj; repo })
          , Some ((3, 17), sprintf "%s organization repo" name_cased) )
        | Gitlab _, _ ->
          User_error.raise
            ~loc
            [ Pp.textf
                "%s repository must be of form user/repo or org/proj/repo"
                name_cased
            ]
        | _, [ _; _; _ ] ->
          User_error.raise
            ~loc
            ~hints:
              [ Pp.textf
                  "The provided form '%s' is specific to %s projects"
                  str
                  (kind_string_noun_case (Gitlab (User_repo { user = ""; repo = "" })))
              ]
            [ Pp.textf "%s repository must be of form user/repo" name_cased ]
        | _, _ ->
          User_error.raise
            ~loc
            [ Pp.textf "%s repository must be of form user/repo" name_cased ]
      in
      let decoder =
        let open Decoder in
        plain_string of_string
        >>= fun (t, since) ->
        (match since with
         | None -> return ()
         | Some (v, what) -> Syntax.since ~what Stanza.syntax v)
        >>> return t
        >>| k
      in
      kind_string kind, decoder)
  ;;

  let encode repo =
    let path =
      match repo with
      | Gitlab (Org_repo { org; proj; repo }) -> sprintf "%s/%s/%s" org proj repo
      | Github { user; repo }
      | Bitbucket { user; repo }
      | Gitlab (User_repo { user; repo })
      | Sourcehut { user; repo }
      | Tangled { user; repo }
      | Codeberg { user; repo } -> sprintf "%s/%s" user repo
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
      | Tangled _ -> base
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
