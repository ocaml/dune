open Import

let group =
  (Term.Group.Group [ in_group Internal_dump.command ], Term.info "internal")
