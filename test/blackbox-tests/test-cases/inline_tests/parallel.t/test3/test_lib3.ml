let () =
  Fake_backend_runner.register ~libname:"other_lib" ~partition:"p1"
    "second test" (fun () -> assert false)
