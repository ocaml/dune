open Stdune

module Env : sig
  (** An opam environment consisting of assignments to variables (e.g. "arch"
      and "os") *)
  type t

  (** An environment containing no variables *)
  val empty : t

  (** Create an environment matching that of the global opam installation. *)
  val global : unit -> t

  (** Adds a value to the environment *)
  val add : var:Env.Var.t -> value:string -> t -> t

  (** [union l r] merges two environments together. On key clashes prefers the
      value from [r]. *)
  val union : t -> t -> t
end

module Repo_selection : sig
  (** An opam repository *)
  type t

  (** A t that does not yet have an environment added. Use [add_env]. *)
  type pre

  (** An opam repo associated with a switch of a given name or directory *)
  val switch_with_name : string -> pre

  (** An opam repo in a local directory given by [opam_repo_dir_path]. Note that
      [opam_repo_dir_path] is a [Filename.t] rather than, say, a
      [Path.Outside_build_dir.t]. This is due to a current limitation in what
      that type (and other modules in [Path]) can represent. It's desirable that
      the [opam_repo_dir_path] argument have the following properties:

      - It should be possible to build a [Repo_selection.t] from command line
        arguments during argument parsing, which precludes the use of a [Path.t]
        here as creating a [Path.t] requires some global state to be set up (by
        [Common.init]) which isn't setup at the time command line arguments are
        parsed.
      - This argument should accept both paths within the source directory and
        external paths, since in practical usage it will usually be pointed to
        some out-of-source copy of the opam repository, but for unit tests it's
        convenient that this support paths within the source directory too. This
        rules out using [Path.External.t], [Path.Source.t], and [Path.Local.t].
      - We want to support relative paths external to the source directory to
        handle the case where someone has the opam repository checkoud out next
        to their project, and runs: `dune pkg lock --opam-repository-path
        ../opam-repository`. This rules out [Path.Outside_build_dir.t] as it
        doesn't permit relative paths outside the source directory. *)
  val local_repo_with_env : opam_repo_dir_path:Filename.t -> env:Env.t -> pre

  val add_env : Env.t -> pre -> t
end

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message :
    t -> lock_dir_path:Path.Source.t -> User_message.t
end

module Version_preference : sig
  type t =
    | Newest
    | Oldest

  val equal : t -> t -> bool

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val default : t

  val all_by_string : (string * t) list

  val decode : t Dune_sexp.Decoder.t
end

val solve_lock_dir :
     version_preference:Version_preference.t
  -> repo_selection:Repo_selection.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
