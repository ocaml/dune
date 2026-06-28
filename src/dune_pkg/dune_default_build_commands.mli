(** Single source of truth for the opam [build:] commands dune emits when it
    generates opam files for its own packages. Used by
    [src/dune_rules/opam_create.ml] for generation and by [Resolved_package]
    to recognise the same shapes at locking time. *)

(** Returns the opam value dune emits for the [build:] field of a generated
    opam file. [exclusive_dir] is the package's exclusive subdirectory if
    any (lang >= 3.23 uses it to scope the [@runtest] target). *)
val default_build_command_value
  :  dune_version:int * int
  -> with_subst:bool
  -> with_sites:bool
  -> exclusive_dir:string option
  -> OpamParserTypes.FullPos.value

(** Whether the opam file's [build:] matches one of the canonical shapes
    emitted by [default_build_command_value] and its [install:], [patches:],
    [substs:], and [build-env:] fields are all empty. The match is
    structurally exact: any divergence from a canonical shape (extra flags,
    reordered args, different filter) disqualifies. The extra-field check
    is needed because returning [true] causes the lockfile to record
    [(build (dune))], which [src/dune_rules/pkg_rules.ml] expands to a bare
    [dune build -p <name>] with no patch, substitution, or environment
    steps. *)
val is_dune_default : OpamFile.OPAM.t -> bool
