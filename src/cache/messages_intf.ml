open Cache_intf
open Stdune

type version =
  { major : int
  ; minor : int
  }

type promotion =
  { key : Digest.t
  ; files : (Path.Build.t * Digest.t) list
  ; metadata : Sexp.t list
  ; repository : int option
  ; duplication : Duplication_mode.t option
  }

type initial = Initial

type outgoing = Outgoing

type incoming = Incoming

type _ message =
  | Lang : version list -> initial message
  | Promote : promotion -> outgoing message
  | SetBuildRoot : Path.t -> outgoing message
  | SetCommonMetadata : Sexp.t list -> outgoing message
  | SetRepos : repository list -> outgoing message
  | Dedup : File.t -> incoming message
