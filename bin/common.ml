open Stdune
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

let prefix_target common s = common.target_prefix ^ s

let set_dirs c =
  if c.root <> Filename.current_dir_name then
    Sys.chdir c.root;
  Path.set_root (Path.External.cwd ());
  Path.set_build_dir (Path.Kind.of_string c.build_dir)

let set_common_other c ~targets =
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces := c.debug_backtraces;
  Clflags.capture_outputs := c.capture_outputs;
  Clflags.diff_command := c.diff_command;
  Clflags.auto_promote := c.auto_promote;
  Clflags.force := c.force;
  Clflags.watch := c.watch;
  Clflags.external_lib_deps_hint :=
    List.concat
      [ ["dune"; "external-lib-deps"; "--missing"]
      ; c.orig_args
      ; targets
      ]

let set_common c ~targets =
  set_dirs c;
  set_common_other c ~targets
