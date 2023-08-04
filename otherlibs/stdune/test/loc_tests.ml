open Stdune

let%expect_test "#7905 - inverted char offsets" =
  let dir = Temp.create Dir ~prefix:"" ~suffix:"loc" in
  let file = Path.relative dir "file.ml" in
  Io.write_file file {|
type t = A | B

let f () () = function
  | A -> ()
|};
  let pos_fname = Path.to_string file in
  let start = { Lexing.pos_fname; pos_lnum = 4; pos_bol = 0; pos_cnum = 14 } in
  let stop = { start with pos_lnum = 5; pos_cnum = 11 } in
  let loc = Loc.create ~start ~stop in
  Format.printf "%a@." Pp.to_fmt (Loc.pp loc);
  let output =
    [%expect.output] |> String.split_lines |> List.tl |> String.concat ~sep:"\n"
  in
  Temp.destroy Dir file;
  print_endline output;
  [%expect {|
    4 | let f () () = function
    5 |   | A -> () |}]
;;
