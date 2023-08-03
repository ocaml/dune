open Import

let temp_dir = lazy (Temp.create Dir ~prefix:"build" ~suffix:"dune")
let temp_dir_value = lazy (Path.to_absolute_filename (Lazy.force temp_dir))

let file ~prefix ~suffix =
  Temp.temp_in_dir File ~dir:(Lazy.force temp_dir) ~suffix ~prefix
;;

let add_to_env env =
  let value = Lazy.force temp_dir_value in
  Env.add env ~var:Env.Var.temp_dir ~value
;;

let destroy = Temp.destroy
let clear () = if Lazy.is_val temp_dir then Temp.clear_dir (Lazy.force temp_dir)
let () = Hooks.End_of_build.always clear
