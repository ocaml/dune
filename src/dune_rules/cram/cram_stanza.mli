open Import

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

type shell_spec =
  | System_shell  (** the default *)
  | Bash_shell
  | Exec_file_shell of String_with_vars.t

val default_shell_spec : shell_spec

type t =
  { loc : Loc.t (* ; dir : Path.t *)
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; shell : shell_spec
  ; enabled_if : Blang.t
  ; locks : Locks.t
  ; package : Package.t option
  }

type Stanza.t += T of t

val decode : t Dune_lang.Decoder.t
