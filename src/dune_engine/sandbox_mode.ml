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

let repr =
  Repr.variant
    "sandbox-mode"
    [ Repr.case0 "None" ~test:(equal None)
    ; Repr.case0 "Symlink" ~test:(equal (Some Symlink))
    ; Repr.case0 "Copy" ~test:(equal (Some Copy))
    ; Repr.case0 "Hardlink" ~test:(equal (Some Hardlink))
    ; Repr.case0 "Patch_back_source_tree" ~test:(equal (Some Patch_back_source_tree))
    ]
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

(* All user-facing modes, excluding patch_back_source_tree. Includes symlink
   on all platforms so that Windows users get a clear error message. *)
let cli_options = [ None; Some Symlink; Some Copy; Some Hardlink ]

(* The order of sandboxing modes in this list determines the order in which Dune
   will try to use them when satisfying sandboxing constraints. On Windows,
   symlink is excluded since it is not supported. *)
let all_except_patch_back_source_tree =
  if Sys.win32
  then List.filter cli_options ~f:(fun m -> m <> Some Symlink)
  else cli_options
;;

let all = all_except_patch_back_source_tree @ [ Some Patch_back_source_tree ]
let none = None
let symlink = Some Symlink
let copy = Some Copy
let hardlink = Some Hardlink

let to_string = function
  | None -> "none"
  | Some Symlink -> "symlink"
  | Some Copy -> "copy"
  | Some Hardlink -> "hardlink"
  | Some Patch_back_source_tree -> "patch_back_source_tree"
;;
