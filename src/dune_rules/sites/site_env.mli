open Import

(** [add_packages_env context ~base dune_file packages]
    Modify [base] in [context] to include the site locations discovered from
    [packages] and [dune_file] *)
val add_packages_env
  :  Context_name.t
  -> base:Env.t
  -> Dune_file.t list
  -> Package.t Dune_lang.Package_name.Map.t
  -> Env.t Memo.t
