open Stdune

type t =
  | Rpc
  | Gc
  | Fd
  | Sandbox
  | Persistent
  | Process
  | Rules
  | Pkg
  | Scheduler
  | Promote
  | Build
  | Debug

let all =
  [ Rpc
  ; Gc
  ; Fd
  ; Sandbox
  ; Persistent
  ; Process
  ; Rules
  ; Pkg
  ; Scheduler
  ; Promote
  ; Build
  ; Debug
  ]
;;

let to_string = function
  | Rpc -> "rpc"
  | Gc -> "gc"
  | Fd -> "fd"
  | Sandbox -> "sandbox"
  | Persistent -> "persistent"
  | Process -> "process"
  | Rules -> "rules"
  | Pkg -> "pkg"
  | Scheduler -> "scheduler"
  | Promote -> "promote"
  | Build -> "build"
  | Debug -> "debug"
;;

let of_string =
  let all = List.map all ~f:(fun a -> to_string a, a) in
  fun x -> List.assoc_opt x all
;;

let to_dyn t = Dyn.variant (String.uppercase_ascii (to_string t)) []

module Set = Bit_set.Make (struct
    type nonrec t = t

    let to_dyn = to_dyn
    let all = all

    let to_int = function
      | Rpc -> 0
      | Gc -> 1
      | Fd -> 2
      | Sandbox -> 3
      | Persistent -> 4
      | Process -> 5
      | Rules -> 6
      | Pkg -> 7
      | Scheduler -> 8
      | Promote -> 9
      | Build -> 10
      | Debug -> 11
    ;;
  end)
