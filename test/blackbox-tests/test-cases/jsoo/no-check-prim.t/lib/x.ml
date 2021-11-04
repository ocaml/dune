module Js = Js_of_ocaml.Js

let buy_it = "buy " ^ Y.it
let print x = Js.to_string (Js.Unsafe.get x "name")
external external_print  : Js.js_string Js.t -> unit = "jsPrint"
