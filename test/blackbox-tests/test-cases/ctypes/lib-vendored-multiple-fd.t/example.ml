let () =
  let r1 = Examplelib.C.Functions_sequential.add2 0 in
  let r2 = Examplelib.C.Functions_unlocked.add4 r1 in
  Printf.printf "%d\n" r2
