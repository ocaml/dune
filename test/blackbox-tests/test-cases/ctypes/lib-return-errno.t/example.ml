let () =
  let r1 = Examplelib.C.Functions_default_policy.add2 0 in
  let ((r2:int), (_:Signed.SInt.t)) =
    Examplelib.C.Functions_return_errno.add4 r1
  in
  Printf.printf "%d\n" r2
