open! Import

type ('k, 'v) t =
  { (* The cache stores results so that if an exception was raised while
       computing a value, concurrent accesses to the cache can find out. *)
    table : ('k, ('v, Exn_with_backtrace.t list) result Fiber.Ivar.t) Table.t
      (* This module is stored so it can be reused to create a table from the cache. *)
  ; key_module : (module Table.Key with type t = 'k)
  }

let create key_module = { table = Table.create key_module 1; key_module }

let find_or_add t key ~f =
  let open Fiber.O in
  let* () = Fiber.return () in
  match Table.find t.table key with
  | Some ivar ->
    let* value_result = Fiber.Ivar.read ivar in
    (match value_result with
     | Ok value -> Fiber.return value
     | Error exns -> Fiber.reraise_all exns)
  | None ->
    let ivar = Fiber.Ivar.create () in
    (* Add the empty ivar to the table before calling [f] so that if the key
       requested again while [f] is running then the caller will wait on the
       ivar that will eventually contain the result. *)
    Table.set t.table key ivar;
    let* value_result = Fiber.collect_errors f in
    let* () = Fiber.Ivar.fill ivar value_result in
    (match value_result with
     | Ok value -> Fiber.return value
     | Error errors -> Fiber.reraise_all errors)
;;

let to_table t =
  let open Fiber.O in
  let* () = Fiber.return () in
  let+ values_by_key =
    Table.foldi t.table ~init:[] ~f:(fun key ivar acc ->
      (let+ value_result = Fiber.Ivar.read ivar in
       match value_result with
       | Ok value -> Some (key, value)
       | Error _ -> None)
      :: acc)
    |> Fiber.all_concurrently
    >>| List.filter_opt
  in
  let table = Table.create t.key_module 1 in
  List.iter values_by_key ~f:(fun (key, value) -> Table.add_exn table key value);
  table
;;
