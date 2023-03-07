open Stdune

let dir = Temp.create Dir ~prefix:"copyfile" ~suffix:"bench"

let contents = String.make 50_000 '0'

let () =
  let src = Path.relative dir "initial" in
  Io.write_file (Path.relative dir "initial") contents;
  let chmod _ = 444 in
  for i = 1 to 10_000 do
    let dst = Path.relative dir (sprintf "dst-%d" i) in
    Io.copy_file ~chmod ~src ~dst ()
  done
