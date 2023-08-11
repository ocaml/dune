open Import

module type S = sig
  type 'a t

  val always : (unit -> unit t) -> unit
  val run : unit -> unit t
end

module Make () : S with type 'a t = 'a = struct
  type 'a t = 'a

  let hooks = ref []
  let always hook = hooks := hook :: !hooks

  let run () =
    let exns = ref [] in
    let run f =
      match Exn_with_backtrace.try_with f with
      | Ok () -> ()
      | Error exn -> exns := exn :: !exns
    in
    List.iter !hooks ~f:run;
    match !exns with
    | [] -> ()
    | exns ->
      let exns = List.rev exns in
      let open Dyn in
      Code_error.raise "hooks failed" [ "exns", (list Exn_with_backtrace.to_dyn) exns ]
  ;;
end

module Make_with_dependencies () : S with type 'a t = 'a Action_builder0.t = struct
  type 'a t = 'a Action_builder0.t

  let hooks = ref []

  let always hook =
    (* We put the [hook] under a bind, to delay its execution. *)
    hooks := Action_builder0.bind (Action_builder0.return ()) ~f:hook :: !hooks
  ;;

  let run () = Action_builder0.all_unit !hooks
end

module Start_of_build = Make_with_dependencies ()
module End_of_build = Make_with_dependencies ()
module Post_build = Make ()

let run_post_build_hook_at_exit = lazy (at_exit Post_build.run)
