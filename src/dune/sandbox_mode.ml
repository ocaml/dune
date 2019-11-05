open! Stdune

type some =
  | Symlink
  | Copy

let compare_some a b =
  match (a, b) with
  | Symlink, Symlink -> Eq
  | Symlink, _ -> Lt
  | _, Symlink -> Gt
  | Copy, Copy -> Eq

type t = some option

let compare = Option.compare compare_some

let equal a b =
  match compare a b with
  | Eq -> true
  | Lt
  | Gt ->
    false

module Dict = struct
  type key = t

  type 'a t =
    { none : 'a
    ; symlink : 'a
    ; copy : 'a
    }

  let compare compare x y =
    let compare_k a b k =
      match compare a b with
      | Eq -> k ()
      | Lt -> Lt
      | Gt -> Gt
    in
    compare_k x.none y.none (fun () ->
        compare_k x.symlink y.symlink (fun () -> compare x.copy y.copy))

  let of_func (f : key -> _) =
    { none = f None; symlink = f (Some Symlink); copy = f (Some Copy) }

  let get { none; symlink; copy } (key : key) =
    match key with
    | None -> none
    | Some Copy -> copy
    | Some Symlink -> symlink
end

module Set = struct
  type key = t

  type t = bool Dict.t

  let compare = Dict.compare Bool.compare

  let equal a b =
    match compare a b with
    | Eq -> true
    | Lt
    | Gt ->
      false

  let of_func = Dict.of_func

  let mem = Dict.get

  let inter (x : t) (y : t) : t =
    { none = x.none && y.none
    ; copy = x.copy && y.copy
    ; symlink = x.symlink && y.symlink
    }
end

(* these should be listed in the default order of preference *)
let all = [ None; Some Symlink; Some Copy ]

let none = None

let symlink = Some Symlink

let copy = Some Copy

let error = Error "invalid sandboxing mode, must be 'none', 'symlink' or 'copy'"

let of_string = function
  | "none" -> Ok None
  | "symlink" -> Ok (Some Symlink : t)
  | "copy" -> Ok (Some Copy)
  | _ -> error

let to_string = function
  | None -> "none"
  | Some Symlink -> "symlink"
  | Some Copy -> "copy"
