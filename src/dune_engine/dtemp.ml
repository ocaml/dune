open Import

let default_temp_dir = lazy (Temp.create Dir ~prefix:"build" ~suffix:"dune")
let temp_dir = ref None
let get_temp_dir () = Option.value !temp_dir ~default:(Lazy.force default_temp_dir)
let file ~prefix ~suffix = Temp.temp_in_dir File ~dir:(get_temp_dir ()) ~suffix ~prefix

let add_to_env env =
  let value = Path.to_absolute_filename (get_temp_dir ()) in
  Env.add env ~var:Env.Var.temp_dir ~value
;;

let with_temp_dir_for_shell dir ~f =
  let previous = !temp_dir in
  temp_dir := Some dir;
  Fiber.finalize f ~finally:(fun () ->
    temp_dir := previous;
    Fiber.return ())
;;

let destroy = Temp.destroy

let clear () =
  match !temp_dir with
  | Some dir -> Temp.clear_dir dir
  | None ->
    if Lazy.is_val default_temp_dir then Temp.clear_dir (Lazy.force default_temp_dir)
;;
