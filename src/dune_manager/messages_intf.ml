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

type message =
  | Lang of version list
  | Promote of promotion
  | SetBuildRoot of Path.t
  | SetCommonMetadata of Sexp.t list
  | SetDuneMemoryRoot of Path.t
  | SetRepos of Dune_memory.repository list
