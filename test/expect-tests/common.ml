open Stdune

let () = Printexc.record_backtrace false

let print pp = Format.printf "%a@." Pp.render_ignore_tags pp
let print_dyn dyn = print (Dyn.pp dyn)
