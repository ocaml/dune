open Import

(** Add dune-site runtime settings and the legacy workspace site locations. *)
val add_packages_env : Context.t -> base:Env.t -> Env.t Memo.t

(** Site locations contributed by [entries] under [root], without fallbacks.
    Share the builder to reuse the computed locations for a layout. If
    locations are contributed, depend on the action's final site binding. *)
val install_env
  :  Context_name.t
  -> root:Path.Build.t
  -> entries:Path.t Install.Entry.Expanded.t Path.Build.Map.t
  -> Env.t Action_builder.t

(** Extend [base] with a contribution, prepending install path variables and
    site locations rather than replacing them. When combining site bindings,
    keep the first occurrence of each location. *)
val extend_env : base:Env.t -> Env.t -> Env.t
