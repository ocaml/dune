open Stdune
open Fiber.O

module Lock = struct
  type t = Path.t

  let create path = Path.build path

  let with_lock t ~f =
    Fiber.of_thunk (fun () ->
        let rec wait_for_lock () =
          match Lock_file.try_create t with
          | None ->
            (* we should wait but circular deps make this hard for now *)
            wait_for_lock ()
          | Some lf -> lf
        in
        let lf = wait_for_lock () in
        let () = Format.eprintf "Locked @.%!" in
        let+ a = f () in
        Lock_file.unlock lf;
        let () = Format.eprintf "Unlocked @.%!" in
        a)
end

let t = Lock.create (Path.Build.of_string "dune.lock")
