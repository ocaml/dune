open Stdune

module type S = sig
  val always : (unit -> unit) -> unit

  val once : (unit -> unit) -> unit

  val run : unit -> unit
end

module Make () = struct
  let persistent_hooks = ref []

  let one_off_hooks = ref []

  let always hook = persistent_hooks := hook :: !persistent_hooks

  let once hook = one_off_hooks := hook :: !one_off_hooks

  let run () =
    let l = !one_off_hooks in
    one_off_hooks := [];
    List.iter l ~f:(fun f -> f ());
    List.iter !persistent_hooks ~f:(fun f -> f ())
end

module End_of_build = Make ()

let () = at_exit End_of_build.run
