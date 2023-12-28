open Import

(** Like [Env_stanza_db.inherited] but specialied for flags. In its own module
    to avoid dependency cycles *)
val flags
  :  name:string
  -> root:(Context_name.t -> Dune_project.t -> 'a Memo.t)
  -> f:(parent:'a Memo.t -> Expander.t -> Dune_env.config -> 'a Memo.t)
  -> (Path.Build.t -> 'a Memo.t) Staged.t
