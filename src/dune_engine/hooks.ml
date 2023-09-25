open Import

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
    let exns = ref [] in
    let run f =
      match Exn_with_backtrace.try_with f with
      | Ok () -> ()
      | Error exn -> exns := exn :: !exns
    in
    List.iter l ~f:run;
    List.iter !persistent_hooks ~f:run;
    match !exns with
    | [] -> ()
    | exns ->
      let exns = List.rev exns in
      let open Dyn in
      Code_error.raise "hooks failed" [ "exns", (list Exn_with_backtrace.to_dyn) exns ]
  ;;
end

module End_of_build = Make ()

let () = at_exit End_of_build.run
