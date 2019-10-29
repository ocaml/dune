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
  }

type outgoing = Outgoing

type incoming = Incoming

type _ message =
  | Lang : version list -> outgoing message
  | Promote : promotion -> outgoing message
  | SetBuildRoot : Path.t -> outgoing message
  | SetCommonMetadata : Sexp.t list -> outgoing message
  | SetRepos : Dune_memory.repository list -> outgoing message
  | Dedup : Dune_memory.File.t -> incoming message
