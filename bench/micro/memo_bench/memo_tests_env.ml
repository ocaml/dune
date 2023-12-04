module Io = struct
  type 'a t = 'a Fiber.t

  let of_thunk f = Fiber.of_thunk f
  let map t ~f = Fiber.map t ~f
  let bind t ~f = Fiber.bind t ~f:(fun x -> f x)
  let return x = Fiber.return x

  module Ivar = struct
    include Fiber.Ivar

    let read x = read x
    let fill x v = fill x v
  end

  module Let_syntax = struct
    let ( let+ ) x f = map x ~f
    let ( let* ) x f = bind x ~f
    let return = return
  end
end

let invalidation_acc = ref Memo.Invalidation.empty

module Memo = struct
  include Memo

  let sample_count =
    (* Count number of samples of all lifted computations, to allow simple
       detection of looping tests executed by [run] *)
    ref 0
  ;;

  let exec build =
    (* not expected to be used in re-entrant way *)
    sample_count := 0;
    Memo.reset !invalidation_acc;
    invalidation_acc := Memo.Invalidation.empty;
    let fiber = Memo.run build in
    Fiber.run fiber ~iter:(fun _ -> failwith "deadlock?")
  ;;

  let of_io f = Memo.of_reproducible_fiber (Fiber.of_thunk f)

  let memoize t =
    let l = Memo.lazy_ ~cutoff:(fun _ _ -> false) (fun () -> t) in
    Memo.of_thunk (fun () -> Memo.Lazy.force l)
  ;;

  let map2 x y ~f =
    map ~f:(fun (x, y) -> f x y) (Memo.fork_and_join (fun () -> x) (fun () -> y))
  ;;

  let all l = Memo.all_concurrently l

  module Glass = struct
    type t = (unit, unit) Memo.Cell.t

    let create () = Memo.lazy_cell ~cutoff:(fun _ _ -> false) (fun () -> Memo.return ())

    let break (t : t) =
      invalidation_acc
      := Memo.Invalidation.combine
           (Memo.Cell.invalidate ~reason:Memo.Invalidation.Reason.Test t)
           !invalidation_acc
    ;;
  end

  let of_glass (g : Glass.t) v =
    Memo.of_thunk (fun () -> Memo.map (Memo.Cell.read g) ~f:(fun () -> v))
  ;;

  let of_thunk f = Memo.of_reproducible_fiber (Fiber.of_thunk (fun () -> Memo.run (f ())))

  module Let_syntax = struct
    let ( let+ ) x f = map x ~f
    let ( let* ) x f = bind x ~f
    let return = return
  end
end

let run tenacious = Memo.exec tenacious

module Glass = Memo.Glass

let make_counter () =
  let r = ref 0 in
  let glass = Glass.create () in
  let break () = Glass.break glass in
  ( Memo.map
      (Memo.of_thunk (fun () -> Memo.Cell.read glass))
      ~f:(fun () ->
        incr r;
        !r)
  , break )
;;

module Var = struct
  type 'a t =
    { value : 'a ref
    ; cell : (unit, 'a) Memo.Cell.t
    }

  let create value =
    let value = ref value in
    { value
    ; cell = Memo.lazy_cell ~cutoff:(fun _ _ -> false) (fun () -> Memo.return !value)
    }
  ;;

  let set t v =
    t.value := v;
    invalidation_acc
    := Memo.Invalidation.combine
         !invalidation_acc
         (Memo.Cell.invalidate ~reason:Memo.Invalidation.Reason.Test t.cell)
  ;;

  let read t = Memo.of_thunk (fun () -> Memo.Cell.read t.cell)
  let peek t = !(t.value)
end
