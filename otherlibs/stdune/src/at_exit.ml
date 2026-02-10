module List = ListLabels

type t =
  { run : unit -> unit
  ; mutable pre : t list
  }

let main = { run = (fun () -> ()); pre = [] }

let run_hook f =
  match Exn_with_backtrace.try_with f with
  | Ok () -> ()
  | Error exn -> Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn
;;

let () =
  at_exit (fun () ->
    let rec loop ({ run; pre } as t) =
      List.iter (List.rev pre) ~f:loop;
      t.pre <- [];
      run_hook run
    in
    loop main)
;;

let at_exit t run =
  let hook = { run; pre = [] } in
  t.pre <- hook :: t.pre;
  hook
;;

let at_exit_ignore t run =
  let (_ : t) = at_exit t run in
  ()
;;
