let () =
  Printf.printf "%d\n" (C.Functions.add2 C.Types.foo_version);
  Printf.printf "%d\n" (C.Functions.add2 C.Types.bar_version);
  Printf.printf "%d\n" (C.Functions.add2 C.Types.baz_version);
  Printf.printf "%d\n" (C.Functions.add2 C.Types.qux_version);
  ()
