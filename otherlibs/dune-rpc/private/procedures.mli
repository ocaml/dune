open Types
open Exported_types

module Public : sig
  val ping : (unit, unit) Decl.Request.t

  val diagnostics : (unit, Diagnostic.t list) Decl.Request.t

  val shutdown : unit Decl.Notification.t

  val format_dune_file :
    (Path.t * [ `Contents of string ], string) Decl.Request.t

  val promote : (Path.t, unit) Decl.Request.t

  val build_dir : (unit, Path.t) Decl.Request.t

  (** Returns the count of the number of builds that have completed since the
      RPC server started. This is intended to be used to find out when builds
      are completed by dune when it's running in watch mode (which also runs an
      RPC server) for the purposes of synchronizing multiple successive changes
      to files to trigger rebuilds in non-interactive settings such as
      benchmarks and tests. *)
  val build_count : (unit, int) Decl.Request.t
end

module Server_side : sig
  val abort : Message.t Decl.Notification.t

  val log : Message.t Decl.Notification.t
end

module Poll : sig
  type 'a t

  val poll : 'a t -> (Id.t, 'a option) Decl.Request.t

  val cancel : 'a t -> Id.t Decl.Notification.t

  module Name : sig
    type t

    val make : string -> t

    val compare : t -> t -> Ordering.t
  end

  val make : Name.t -> (Id.t, 'a option) Decl.Request.gen list -> 'a t

  val name : 'a t -> Name.t

  val progress : Progress.t t

  val diagnostic : Diagnostic.Event.t list t
end
