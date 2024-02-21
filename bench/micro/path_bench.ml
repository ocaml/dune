module Path = Stdune.Path
module Fpath = Stdune.Fpath
open Base
module Filename = Stdlib.Filename

let () = Path.Build.set_build_dir (In_source_dir Path.Source.(relative root "_build"))
let root = "."
let short_path = "a/b/c"
let long_path = List.init 20 ~f:(fun _ -> "foo-bar-baz") |> String.concat ~sep:"/"

let%bench_fun ("is_root" [@params
                           path
                           = [ "root", "."
                             ; "short path", short_path
                             ; "long path", long_path
                             ]])
  =
  fun () -> ignore (Fpath.is_root path)
;;

let%bench_fun ("reach" [@params
                         t
                         = [ "from root long path", (long_path, root)
                           ; "from root short path", (short_path, root)
                           ; "reach root from short path", (root, short_path)
                           ; "reach root from long path", (root, long_path)
                           ; ( "reach long path from similar long path"
                             , ( Filename.concat long_path "a"
                               , Filename.concat long_path "b" ) )
                           ; ( "reach short path from similar short path"
                             , ( Filename.concat short_path "a"
                               , Filename.concat short_path "b" ) )
                           ]])
  =
  let t, from = t in
  let t = Path.of_string t in
  let from = Path.of_string from in
  fun () -> ignore (Path.reach t ~from)
;;

let%bench_fun ("Path.Local.relative" [@params
                                       t
                                       = [ "left root", (".", long_path)
                                         ; "right root", (long_path, ".")
                                         ; "short paths", (short_path, short_path)
                                         ; "long paths", (long_path, long_path)
                                         ]])
  =
  let x, y = t in
  let x = Path.Local.of_string x in
  fun () -> ignore (Path.Local.relative x y)
;;

let%bench_fun ("Path.Local.append" [@params
                                     t
                                     = [ "left root", (".", long_path)
                                       ; "right root", (long_path, ".")
                                       ; "short paths", (short_path, short_path)
                                       ; "long paths", (long_path, long_path)
                                       ]])
  =
  let x, y = t in
  let x = Path.Local.of_string x in
  let y = Path.Local.of_string y in
  fun () -> ignore (Path.Local.append x y)
;;
