type format = Text | JSON

val describe :
  Dune_project.t -> Dune_load.Dune_file.t list -> format:format -> unit
