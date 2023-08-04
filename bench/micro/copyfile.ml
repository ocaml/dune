open Stdune

let dir =
  (if Array.length Sys.argv > 1
   then (
     let dir = Path.of_filename_relative_to_initial_cwd Sys.argv.(1) in
     Temp.temp_in_dir Dir ~dir)
   else Temp.create Dir)
    ~prefix:"copyfile"
    ~suffix:"bench"
;;

let contents =
  let len =
    if Array.length Sys.argv > 2 then Int.of_string_exn Sys.argv.(2) else 50_000
  in
  String.make len '0'
;;

let () =
  let src = Path.relative dir "initial" in
  Io.write_file (Path.relative dir "initial") contents;
  let chmod _ = 444 in
  for i = 1 to 10_000 do
    let dst = Path.relative dir (sprintf "dst-%d" i) in
    Io.copy_file ~chmod ~src ~dst ()
  done
;;
