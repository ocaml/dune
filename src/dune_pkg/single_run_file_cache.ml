open! Import

type 'error t =
  { temp_dir : Path.t lazy_t
  ; key_to_filename : (string, (Path.t, 'error) result) Fiber_cache.t
  ; mutable num_entries : int
  }

let create () =
  let temp_dir =
    lazy
      (let temp_dir = Temp.create Dir ~prefix:"dune" ~suffix:"single-run-file-cache" in
       at_exit (fun () -> Temp.destroy Dir temp_dir);
       temp_dir)
  in
  { temp_dir; key_to_filename = Fiber_cache.create (module String); num_entries = 0 }
;;

let with_ ({ temp_dir; key_to_filename; _ } as t) ~key ~f =
  let open Fiber.O in
  let temp_dir = Lazy.force temp_dir in
  Fiber_cache.find_or_add key_to_filename key ~f:(fun () ->
    let filename = string_of_int t.num_entries in
    t.num_entries <- t.num_entries + 1;
    let output_path = Path.relative temp_dir filename in
    let+ result = f output_path in
    match result with
    | Ok () ->
      if not (Path.exists output_path)
      then
        Code_error.raise
          "Callback failed to create file"
          [ "output", Path.to_dyn output_path; "key", Dyn.string key ];
      Ok (Path.relative temp_dir filename)
    | Error _ as e -> e)
;;
