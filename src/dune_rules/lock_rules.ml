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

  let action { target; lock_dir } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    Printf.printf "Our ACTIOn target is %s, our lock_dir is %S\n" (Path.Build.to_string target) lock_dir;
    let path = Path.build target in
    Io.write_file ~binary:true path "Hello I exist";
    ()
  ;;
end

module A = Action_ext.Make (Spec)

let action ~target ~lock_dir = A.action { Spec.target; lock_dir }

let lock ~target ~lock_dir =
  Printf.printf "Setting up action for target(%S)\n" (Path.Build.to_string target);
  let file_target = Path.Build.relative target "lock.dune" in
  action ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:true
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add ~file_targets:[ file_target ]
  (* we might not need the directory target *)
  (* |> Action_builder.With_targets.add_directories ~directory_targets:[ target ] *)
;;
