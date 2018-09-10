open Dune

type t =
  { debug_dep_path        : bool
  ; debug_findlib         : bool
  ; debug_backtraces      : bool
  ; profile               : string option
  ; workspace_file        : Arg.Path.t option
  ; root                  : string
  ; target_prefix         : string
  ; only_packages         : Package.Name.Set.t option
  ; capture_outputs       : bool
  ; x                     : string option
  ; diff_command          : string option
  ; auto_promote          : bool
  ; force                 : bool
  ; ignore_promoted_rules : bool
  ; build_dir             : string
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args             : string list
  ; config                : Config.t
  ; default_target        : string
  (* For build & runtest only *)
  ; watch : bool
  }

val prefix_target : t -> string -> string

val set_common : t -> targets:string list -> unit

val set_common_other : t -> targets:string list -> unit

val set_dirs : t -> unit
