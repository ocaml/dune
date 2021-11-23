open Stdune
open Fiber.O

let n = 1000

let%bench_fun "bind" =
 fun () ->
  Fiber.run
    ~iter:(fun () -> assert false)
    (let rec loop = function
       | 0 -> Fiber.return ()
       | n -> Fiber.return () >>= fun () -> loop (n - 1)
     in
     loop n)

let%bench_fun "ivar" =
 fun () ->
  let ivar = ref (Fiber.Ivar.create ()) in
  Fiber.run
    ~iter:(fun () ->
      let open Nonempty_list in
      [ Fiber.Fill (!ivar, ()) ])
    (let rec loop = function
       | 0 -> Fiber.return ()
       | n ->
         ivar := Fiber.Ivar.create ();
         let* () = Fiber.Ivar.read !ivar in
         loop (n - 1)
     in
     loop n)

let%bench_fun "Var.set" =
  let var = Fiber.Var.create () in
  fun () ->
    Fiber.run
      ~iter:(fun () -> assert false)
      (let rec loop = function
         | 0 -> Fiber.return ()
         | n -> Fiber.Var.set var n (fun () -> loop (n - 1))
       in
       loop n)

let%bench_fun "Var.get" =
  let var = Fiber.Var.create () in
  fun () ->
    Fiber.run
      ~iter:(fun () -> assert false)
      (let rec loop = function
         | 0 -> Fiber.return ()
         | n ->
           let* (_ : int option) = Fiber.Var.get var in
           loop (n - 1)
       in
       Fiber.Var.set var 0 (fun () -> loop n))

let exns =
  List.init n ~f:(fun _ ->
      { Exn_with_backtrace.exn = Exit
      ; backtrace = Printexc.get_raw_backtrace ()
      })

let%bench "catching exceptions" =
  Fiber.run
    ~iter:(fun () -> assert false)
    (Fiber.map_reduce_errors
       (module Monoid.Unit)
       ~on_error:(fun _ -> Fiber.return ())
       (fun () -> Fiber.reraise_all exns))
  |> ignore

let%bench "installing handlers" =
  Fiber.run
    ~iter:(fun () -> assert false)
    (let rec loop = function
       | 0 -> Fiber.return ()
       | n ->
         let* (_ : (unit, unit) result) =
           Fiber.map_reduce_errors
             (module Monoid.Unit)
             ~on_error:(fun _ -> Fiber.return ())
             (fun () -> Fiber.return ())
         in
         loop (n - 1)
     in
     loop n)
  |> ignore
