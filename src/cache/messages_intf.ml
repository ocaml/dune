open Cache_intf
open Stdune

(** A version of the communication protocol between Dune and the cache daemon. *)
type version =
  { major : int
  ; minor : int
  }

(** When Dune successfully executes a build rule, it sends a "promotion" message
    to the cache daemon, listing the produced [files] along with some [metadata]
    and a few other fields relevant for caching. *)
type promotion =
  { key : Key.t
  ; files : (Path.Build.t * Digest.t) list
  ; metadata : Sexp.t list
  ; repository : int option
  ; duplication : Duplication_mode.t option
  }

(** There is one initial message [Lang], which is sent by Dune and the cache
    daemon to each other during the initial negotiation of the version of the
    communication protocol. *)
type initial = Initial

(** Outgoing messages are sent by Dune to the cache daemon. *)
type outgoing = Outgoing

(** Incoming messages are sent by the cache daemon to Dune. *)
type incoming = Incoming

(** Messages of the communication protocol between Dune and the cache daemon. *)
type _ message =
  | Lang : version list -> initial message
      (** Inform the other party about the supported versions of the
          communication protocol. *)
  | SetBuildRoot : Path.t -> outgoing message
      (** Set the absolute path to the build root, to be used when interpreting
          relative paths in subsequent messages. *)
  | SetCommonMetadata : Sexp.t list -> outgoing message
      (** Set the common metadata that should be added to the subsequent
          [Promote] messages. *)
  | SetRepos : repository list -> outgoing message
      (** Set the paths to all the version controlled repositories in the
          workspace along with the associated commit identifiers. *)
  | Promote : promotion -> outgoing message
      (** Promote files produced by a build rule into the cache. *)
  | Hint : Digest.t list -> outgoing message
      (** The cache daemon a rule is going to be built *)
  | Dedup : File.t -> incoming message
      (** Inform Dune that a file that was previously promoted can now be
          replaced by a hardlink to the corresponding file stored in cache. *)
