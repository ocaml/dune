open! Import
open! Stdune

type entry =
  { filename : Filename.t
  ; complete : unit Fiber.Ivar.t
  }

type t =
  { temp_dir : Path.t lazy_t
  ; key_to_filename : entry String.Table.t
  }

let create () =
  let temp_dir =
    lazy
      (let temp_dir = Temp.create Dir ~prefix:"dune" ~suffix:"single-run-file-cache" in
       at_exit (fun () -> Temp.destroy Dir temp_dir);
       temp_dir)
  in
  { temp_dir; key_to_filename = String.Table.create 1 }
;;

let with_ { temp_dir; key_to_filename } ~key ~f =
  let open Fiber.O in
  let temp_dir = Lazy.force temp_dir in
  match String.Table.find key_to_filename key with
  | Some { filename; complete } ->
    (* If the file is currently being created then this ivar will be empty.
       Wait for it to become filled, indicating that the file is ready. *)
    let+ () = Fiber.Ivar.read complete in
    Ok (Path.relative temp_dir filename)
  | None ->
    let filename = string_of_int (String.Table.length key_to_filename) in
    let output_path = Path.relative temp_dir filename in
    let complete = Fiber.Ivar.create () in
    String.Table.add_exn key_to_filename key { filename; complete };
    let* result = f output_path in
    (* Fill the ivar signaling that the file is read. *)
    let+ () = Fiber.Ivar.fill complete () in
    (match result with
     | Ok () ->
       if not (Path.exists output_path)
       then
         Code_error.raise
           "Callback failed to create file"
           [ "output", Path.to_dyn output_path; "key", Dyn.string key ];
       Ok output_path
     | Error _ as e -> e)
;;
