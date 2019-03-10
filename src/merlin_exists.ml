open Stdune

let path ~dir = Path.relative dir ".merlin-exists"
let dep ~dir = Build.path (path ~dir)
let create ~dir = Build.create_file (path ~dir)
