open Stdune
open Fiber.O

let%bench_fun ("bind" [@indexed jobs = [ 100; 10_000; 1_000_000 ]]) =
 fun () ->
  Fiber.run
    ~iter:(fun () -> assert false)
    (let rec loop = function
       | 0 -> Fiber.return ()
       | n -> Fiber.return () >>= fun () -> loop (n - 1)
     in
     loop jobs)

let%bench_fun ("ivar" [@indexed jobs = [ 100; 10_000; 1_000_000 ]]) =
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
     loop jobs)

let%bench_fun ("Var.set" [@indexed jobs = [ 100; 10_000; 1_000_000 ]]) =
  let var = Fiber.Var.create () in
  fun () ->
    Fiber.run
      ~iter:(fun () -> assert false)
      (let rec loop = function
         | 0 -> Fiber.return ()
         | n -> Fiber.Var.set var n (fun () -> loop (n - 1))
       in
       loop jobs)

let%bench_fun ("Var.get" [@indexed jobs = [ 100; 10_000; 1_000_000 ]]) =
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
       Fiber.Var.set var 0 (fun () -> loop jobs))

let exns =
  List.init 100 ~f:(fun _ ->
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
     loop 1000)
  |> ignore
