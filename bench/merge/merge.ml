let read path = Yojson.Safe.from_file path |> Bench_format.t_of_yojson

let merge a b =
  let open Bench_format in
  { a with results = a.results @ b.results }

let write m =
  Bench_format.yojson_of_t m |> Yojson.Safe.pretty_to_channel Stdlib.stdout

let () =
  match Sys.argv with
  | [| _; a_path; b_path |] ->
    let a = read a_path in
    let b = read b_path in
    let m = merge a b in
    write m
  | _ -> assert false
