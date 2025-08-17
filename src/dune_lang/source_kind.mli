open Import

module Host : sig
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

  val homepage : t -> string
  val bug_reports : t -> string
end

type t =
  | Host of Host.t
  | Url of string

val to_dyn : t Dyn.builder
val to_string : t -> string
val decode : t Decoder.t
val encode : t Encoder.t
