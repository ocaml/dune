module Js = Js_of_ocaml.Js

let _ =
  print_endline X.buy_it;
  let obj = Js.Unsafe.obj [|"name", Js.Unsafe.inject (Js.string Z.use_it)|] in
  Printf.printf "%s\n%!" (X.print obj);
  X.external_print (Js.string "break it");
  (fun x ->
     let global = Js.Unsafe.get Js.Unsafe.global "global" in
     let globalPrintFunction : Js.js_string Js.t -> unit =
       Js.Unsafe.get global "globalPrintFunction" in
     globalPrintFunction x
     ) (Js.string "fix it")
