open Import

type some =
  | Symlink
  | Copy
  | Hardlink
  | Patch_back_source_tree

let compare_some a b =
  match a, b with
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
    | Hardlink -> Variant ("Hardlink", [])
    | Patch_back_source_tree -> Variant ("Patch_back_source_tree", []))
;;

module Dict = struct
  type key = t

  type 'a t =
    { none : 'a
    ; symlink : 'a
    ; copy : 'a
    ; hardlink : 'a
    ; patch_back_source_tree : 'a
    }

  let compare compare { none; symlink; copy; hardlink; patch_back_source_tree } t =
    let open Ordering.O in
    let= () = compare none t.none in
    let= () = compare symlink t.symlink in
    let= () = compare copy t.copy in
    let= () = compare hardlink t.hardlink in
    compare patch_back_source_tree t.patch_back_source_tree
  ;;

  let of_func (f : key -> _) =
    { none = f None
    ; symlink = f (Some Symlink)
    ; copy = f (Some Copy)
    ; hardlink = f (Some Hardlink)
    ; patch_back_source_tree = f (Some Patch_back_source_tree)
    }
  ;;

  let get { none; symlink; copy; hardlink; patch_back_source_tree } (key : key) =
    match key with
    | None -> none
    | Some Copy -> copy
    | Some Symlink -> symlink
    | Some Hardlink -> hardlink
    | Some Patch_back_source_tree -> patch_back_source_tree
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
      | Some Patch_back_source_tree -> 4
    ;;

    let all =
      [ None; Some Copy; Some Symlink; Some Hardlink; Some Patch_back_source_tree ]
    ;;

    let to_dyn = to_dyn
  end

  include Bit_set.Make (T)

  (* CR-someday amokhov: [Patch_back_source_tree] is a bit special in that it
     can only appear as a singleton. Perhaps, it should be treated differently
     than other sandboxing modes to make meaningless states
     non-representable. *)
  let patch_back_source_tree_only = singleton (Some Patch_back_source_tree)
  let is_patch_back_source_tree_only t = t = patch_back_source_tree_only
end

(* The order of sandboxing modes in this list determines the order in which Dune
   will try to use them when satisfying sandboxing constraints. *)
let all_except_patch_back_source_tree =
  if Sys.win32
  then [ None; Some Copy; Some Symlink; Some Hardlink ]
  else [ None; Some Symlink; Some Copy; Some Hardlink ]
;;

let all = all_except_patch_back_source_tree @ [ Some Patch_back_source_tree ]
let none = None
let symlink = Some Symlink
let copy = Some Copy
let hardlink = Some Hardlink

let decode =
  let open Dune_sexp.Decoder in
  enum
    [ "none", None
    ; "symlink", Some Symlink
    ; "copy", Some Copy
    ; "hardlink", Some Hardlink
    ]
;;

let to_string = function
  | None -> "none"
  | Some Symlink -> "symlink"
  | Some Copy -> "copy"
  | Some Hardlink -> "hardlink"
  | Some Patch_back_source_tree -> "patch_back_source_tree"
;;
