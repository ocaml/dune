type t =
  { debug_dep_path        : bool
  ; debug_findlib         : bool
  ; debug_backtraces      : bool
  ; profile               : string option
  ; workspace_file        : Arg.Path.t option
  ; root                  : Workspace_root.t
  ; target_prefix         : string
  ; only_packages         : Dune.Package.Name.Set.t option
  ; capture_outputs       : bool
  ; x                     : string option
  ; diff_command          : string option
  ; auto_promote          : bool
  ; force                 : bool
  ; ignore_promoted_rules : bool
  ; build_dir             : string
  ; no_print_directory    : bool
  ; store_orig_src_dir    : bool
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args             : string list
  ; config                : Dune.Config.t
  ; default_target        : string
  (* For build & runtest only *)
  ; watch : bool
  ; stats_trace_file : string option
  }

val prefix_target : t -> string -> string

(** [set_common common ~targets] is [set_dirs common] followed by
    [set_common_other common ~targets]. In general, [set_common] executes
    sequence of side-effecting actions to initialize Dune's working
    environment based on the options determined in a [Common.t] record *)
val set_common : t -> targets:string list -> unit

(** [set_common_other common ~targets] sets all stateful values dictated by
    [common], except those accounted for by [set_dirs]. [targets] are
    used to obtain external library dependency hints, if needed. *)
val set_common_other : t -> targets:string list -> unit

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

val default_build_dir : string

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t
  val ( and+ )
:  'a Cmdliner.Term.t
-> 'b Cmdliner.Term.t
-> ('a * 'b) Cmdliner.Term.t
end
