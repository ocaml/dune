open Import

type some =
  | Symlink
  | Copy
  | Hardlink

let compare_some a b =
  match a, b with
  | Symlink, Symlink -> Eq
  | Symlink, _ -> Lt
  | _, Symlink -> Gt
  | Copy, Copy -> Eq
  | Copy, _ -> Lt
  | _, Copy -> Gt
  | Hardlink, Hardlink -> Eq
;;

type t = some option

let compare = Option.compare compare_some

let equal a b =
  match compare a b with
  | Eq -> true
  | Lt | Gt -> false
;;

let to_dyn =
  Dyn.option (function
    | Symlink -> Variant ("Symlink", [])
    | Copy -> Variant ("Copy", [])
    | Hardlink -> Variant ("Hardlink", []))
;;

module Dict = struct
  type key = t

  type 'a t =
    { none : 'a
    ; symlink : 'a
    ; copy : 'a
    ; hardlink : 'a
    }

  let compare compare { none; symlink; copy; hardlink } t =
    let open Ordering.O in
    let= () = compare none t.none in
    let= () = compare symlink t.symlink in
    let= () = compare copy t.copy in
    compare hardlink t.hardlink
  ;;

  let of_func (f : key -> _) =
    { none = f None
    ; symlink = f (Some Symlink)
    ; copy = f (Some Copy)
    ; hardlink = f (Some Hardlink)
    }
  ;;

  let get { none; symlink; copy; hardlink } (key : key) =
    match key with
    | None -> none
    | Some Copy -> copy
    | Some Symlink -> symlink
    | Some Hardlink -> hardlink
  ;;
end

module Set = struct
  module T = struct
    type nonrec t = t

    let to_int = function
      | None -> 0
      | Some Copy -> 1
      | Some Symlink -> 2
      | Some Hardlink -> 3
    ;;

    let all = [ None; Some Copy; Some Symlink; Some Hardlink ]
    let to_dyn = to_dyn
  end

  include Bit_set.Make (T)
end

(* The order of sandboxing modes in this list determines the order in which Dune
   will try to use them when satisfying sandboxing constraints. *)
let all =
  if Sys.win32
  then [ None; Some Copy; Some Symlink; Some Hardlink ]
  else [ None; Some Symlink; Some Copy; Some Hardlink ]
;;

let none = None
let symlink = Some Symlink
let copy = Some Copy
let hardlink = Some Hardlink

let to_string = function
  | None -> "none"
  | Some Symlink -> "symlink"
  | Some Copy -> "copy"
  | Some Hardlink -> "hardlink"
;;
