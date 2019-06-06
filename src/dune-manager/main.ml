open Stdune
open Dune_manager

let () =
  let manager = DuneManager.make (Path.of_string "/tmp/dune/memory") in
  DuneManager.run manager
