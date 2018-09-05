open Stdune

module End_of_build = struct
  let persistent_hooks = ref []

  let one_off_hooks = ref []

  let always hook =
    persistent_hooks := hook :: !persistent_hooks

  let once hook =
    one_off_hooks := hook :: !one_off_hooks

  let run () =
    List.iter !one_off_hooks ~f:(fun f -> f ());
    List.iter !persistent_hooks ~f:(fun f -> f ());
    one_off_hooks := []
end

let () = at_exit End_of_build.run
