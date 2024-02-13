module Host : sig
  type kind =
    | Github
    | Bitbucket
    | Gitlab
    | Sourcehut

  type t =
    { user : string
    ; repo : string
    ; kind : kind
    }

  val homepage : t -> string
  val bug_reports : t -> string
end

type t =
  | Host of Host.t
  | Url of string

val to_dyn : t Dyn.builder
val to_string : t -> string
val decode : t Dune_sexp.Decoder.t
val encode : t Dune_sexp.Encoder.t
