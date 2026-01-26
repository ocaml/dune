let () =
  Fake_backend_runner.register ~libname:"test_lib2" ~partition:"p1" "first test"
    (fun () -> assert true)

let () =
  Fake_backend_runner.register ~libname:"test_lib2" ~partition:"p1" "second test"
    (fun () -> ()    )

let () =
  Fake_backend_runner.register ~libname:"test_lib2" ~partition:"p2" "first test"
    (fun () -> ())

let () =
  Fake_backend_runner.register ~libname:"test_lib2" ~partition:"p3" "first test"
    (fun () -> failwith "This failure is expected")

let () =
  Fake_backend_runner.register ~libname:"other_lib" ~partition:"p1"
    "second test" (fun () -> assert false)

let () =
  Fake_backend_runner.register ~libname:"other_lib" ~partition:"pn"
    "second test" (fun () -> assert false)
