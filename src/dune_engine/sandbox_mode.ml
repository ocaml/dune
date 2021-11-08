open! Stdune

type some =
  | Symlink
  | Copy
  | Hardlink
  | Patch_back_source_tree

let compare_some a b =
  match (a, b) with
  | Symlink, Symlink -> Eq
  | Symlink, _ -> Lt
  | _, Symlink -> Gt
  | Copy, Copy -> Eq
  | Copy, _ -> Lt
  | _, Copy -> Gt
  | Hardlink, Hardlink -> Eq
  | Hardlink, _ -> Lt
  | _, Hardlink -> Gt
  | Patch_back_source_tree, Patch_back_source_tree -> Eq

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
    ; hardlink : 'a
    ; patch_back_source_tree : 'a
    }

  let compare compare { none; symlink; copy; hardlink; patch_back_source_tree }
      t =
    let ( let= ) ordering k =
      match ordering with
      | Eq -> k ()
      | Lt
      | Gt ->
        ordering
    in
    let= () = compare none t.none in
    let= () = compare symlink t.symlink in
    let= () = compare copy t.copy in
    let= () = compare hardlink t.hardlink in
    let= () = compare patch_back_source_tree t.patch_back_source_tree in
    Eq

  let of_func (f : key -> _) =
    { none = f None
    ; symlink = f (Some Symlink)
    ; copy = f (Some Copy)
    ; hardlink = f (Some Hardlink)
    ; patch_back_source_tree = f (Some Patch_back_source_tree)
    }

  let get { none; symlink; copy; hardlink; patch_back_source_tree } (key : key)
      =
    match key with
    | None -> none
    | Some Copy -> copy
    | Some Symlink -> symlink
    | Some Hardlink -> hardlink
    | Some Patch_back_source_tree -> patch_back_source_tree
end

module Set = struct
  type key = t

  type t = bool Dict.t

  let compare = Dict.compare Bool.compare

  let of_func = Dict.of_func

  let singleton k = of_func (equal k)

  let equal a b =
    match compare a b with
    | Eq -> true
    | Lt
    | Gt ->
      false

  let mem = Dict.get

  let inter (x : t) (y : t) : t =
    { none = x.none && y.none
    ; copy = x.copy && y.copy
    ; symlink = x.symlink && y.symlink
    ; hardlink = x.hardlink && y.hardlink
    ; patch_back_source_tree =
        x.patch_back_source_tree && y.patch_back_source_tree
    }

  let to_dyn { Dict.none; copy; symlink; hardlink; patch_back_source_tree } =
    Dyn.Record
      [ ("none", Bool none)
      ; ("copy", Bool copy)
      ; ("symlink", Bool symlink)
      ; ("hardlink", Bool hardlink)
      ; ("patch_back_source_tree", Bool patch_back_source_tree)
      ]
end

(* these should be listed in the default order of preference *)
let all_except_patch_back_source_tree =
  [ None; Some Symlink; Some Copy; Some Hardlink ]

let all = all_except_patch_back_source_tree @ [ Some Patch_back_source_tree ]

let none = None

let symlink = Some Symlink

let copy = Some Copy

let hardlink = Some Hardlink

let error =
  Error
    "invalid sandboxing mode, must be 'none', 'symlink', 'copy' or 'hardlink'"

let of_string_except_patch_back_source_tree = function
  | "none" -> Ok None
  | "symlink" -> Ok (Some Symlink)
  | "copy" -> Ok (Some Copy)
  | "hardlink" -> Ok (Some Hardlink)
  | _ -> error

let to_string = function
  | None -> "none"
  | Some Symlink -> "symlink"
  | Some Copy -> "copy"
  | Some Hardlink -> "hardlink"
  | Some Patch_back_source_tree -> "patch_back_source_tree"

let to_dyn =
  Dyn.Encoder.option (function
    | Symlink -> Variant ("Symlink", [])
    | Copy -> Variant ("Copy", [])
    | Hardlink -> Variant ("Hardlink", [])
    | Patch_back_source_tree -> Variant ("Patch_back_source_tree", []))
