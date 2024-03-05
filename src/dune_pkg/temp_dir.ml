open Stdune

let in_build =
  lazy
    (let dir = Path.relative (Path.build Path.Build.root) ".temp" in
     Path.mkdir_p dir;
     dir)
;;
