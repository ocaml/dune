type t

val workspace_file : t -> Arg.Path.t option
val x : t -> string option
val profile : t -> Dune.Profile.t option
val capture_outputs : t -> bool
val root : t -> Workspace_root.t
val config : t -> Dune.Config.t
val only_packages : t -> Dune.Package.Name.Set.t option
val watch : t -> bool
val default_target : t -> Arg.Dep.t

val prefix_target : t -> string -> string

(** [set_common common ~targets] is [set_dirs common] followed by
    [set_common_other common ~targets]. In general, [set_common] executes
    sequence of side-effecting actions to initialize Dune's working
    environment based on the options determined in a [Common.t] record *)
val set_common : t -> targets:Arg.Dep.t list -> unit

(** [set_common_other common ~targets] sets all stateful values dictated by
    [common], except those accounted for by [set_dirs]. [targets] are
    used to obtain external library dependency hints, if needed. *)
val set_common_other : t -> targets:Arg.Dep.t list -> unit

(** [set_dirs common] sets the workspace root and build directories, and makes
 the root the current working directory *)
val set_dirs : t -> unit

val help_secs
  : [> `Blocks of [> `P of string | `S of string ] list
    | `P of string
    | `S of string ] list

val footer : [> `Blocks of [> `P of string | `S of string ] list ]

val term : t Cmdliner.Term.t

val context_arg : doc:string -> string Cmdliner.Term.t

(** A [--build-info] command line argument that print build
    informations (included in [term]) *)
val build_info : unit Cmdliner.Term.t

val default_build_dir : string

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t
  val ( and+ )
   :  'a Cmdliner.Term.t
   -> 'b Cmdliner.Term.t
   -> ('a * 'b) Cmdliner.Term.t
end
