open Stdune
open Core
include Univ_map.Key

let get var k = Get_var (var, k)

let get_exn var =
  map (get var) ~f:(function
    | None -> failwith "Fiber.Var.get_exn"
    | Some value -> value)
;;

let set var x f k = Set_var (var, x, fun () -> f () (fun x -> Unwind (k, x)))
let unset var f k = Unset_var (var, fun () -> f () (fun x -> Unwind (k, x)))
let create () = create ~name:"var" (fun _ -> Dyn.string "var")
