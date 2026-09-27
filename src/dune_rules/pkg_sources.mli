open Import

val root_dir_basename : string
val package_of_artifact_path : Path.Build.t -> Package.Name.t option

val add_artifact_source_rules
  :  dir:Path.Build.t
  -> source_dir:Source_tree.Rules.Dir.t
  -> Build_config.Gen_rules.t
  -> Build_config.Gen_rules.t
