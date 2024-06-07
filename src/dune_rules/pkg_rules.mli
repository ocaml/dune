(** rules for packages built by dune *)

open Import

(** The package management rules setup two rules for every package:

    - A rule for fetching the source to produce .pkg/$package/source

    - A rule to build the package and produce the artifacts in
      .pkg/$package/target *)

val setup_rules
  :  components:string list
  -> dir:Path.Build.t
  -> Context_name.t
  -> Build_config.Gen_rules.t Memo.t

val lock_dir_path : Context_name.t -> Path.Source.t option Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val ocaml_toolchain : Context_name.t -> Ocaml_toolchain.t Action_builder.t option Memo.t
val which : Context_name.t -> (Filename.t -> Path.t option Memo.t) Staged.t
val exported_env : Context_name.t -> Env.t Memo.t
val ocamlpath : Context_name.t -> Path.t list Memo.t
val find_package : Context_name.t -> Package.Name.t -> unit Action_builder.t option Memo.t
