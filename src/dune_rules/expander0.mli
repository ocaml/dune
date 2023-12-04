open Import

val isn't_allowed_in_this_position : source:Dune_lang.Template.Pform.t -> 'a
val as_in_build_dir : what:string -> loc:Loc.t -> Path.t -> Path.Build.t
