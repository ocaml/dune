external bazexe : unit -> int = "bazexe"
external hello_world_bazexe : unit -> unit = "hello_world_bazexe"

let () = Quad.hello (); hello_world_bazexe ()


let () = Printf.printf "%d\n%d\n"
  (Quad.quad ())
  (bazexe ())
