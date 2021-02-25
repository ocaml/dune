open! Stdune
open! Notty

let backend (_ : Engine.t) : (module Console.Backend.S) =
  let module Backend : Console.Backend.S = struct
    let print_user_message _ = ()

    let set_status_line _ = ()

    let reset () = ()
  end in
  (module Backend)

let main e =
  let backend = backend e in
  Console.Backend.set backend
