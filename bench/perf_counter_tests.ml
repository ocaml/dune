open Stdune

let () =
  let path = Temp.create File ~prefix:"perf" ~suffix:"stat" in
  Io.write_file path "# started on ...\n\n123456;;instructions:u;100.00;100.00;;\n";
  let instructions = Perf_counter.read_instructions path in
  if instructions <> 123_456
  then
    Code_error.raise
      "incorrect perf instruction count"
      [ "actual", Dyn.int instructions; "expected", Dyn.int 123_456 ]
;;
