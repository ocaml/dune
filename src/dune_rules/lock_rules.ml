open Import

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; lock_dir } _encode_path encode_target : Sexp.t =
    Sexp.List [ encode_target target; Sexp.Atom lock_dir ]
  ;;

  let action { target = _; lock_dir = _ } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    ()
  ;;
end

module A = Action_ext.Make (Spec)

let action ~target ~lock_dir = A.action { Spec.target; lock_dir }

let lock ~target ~lock_dir =
  action ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:true
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;
