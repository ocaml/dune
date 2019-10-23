open Stdune

(** Upgrade string with variables coming from a [jbuild] file to one suitable
    for a [dune] file. Fail if the [<] variable is found and
    [allow_first_dep_var] is [true]. *)
val upgrade_to_dune :
     string
  -> loc:Loc.t
  -> quoted:bool
  -> allow_first_dep_var:bool
  -> Dune_lang.Template.t
