open! Stdune

(* ['a t] represents a total map from [Sandbox_mode.t] to ['a] *)
type 'a gen = {
  none : 'a;
  symlink : 'a;
  copy : 'a;
}

type t = bool gen

let compare_gen compare x y =
  let compare_k a b k =
    match compare a b with
    | Eq -> k ()
    | Lt -> Lt
    | Gt -> Gt
  in
  compare_k x.none y.none (fun () ->
    compare_k x.symlink y.symlink (fun () ->
      compare x.copy y.copy
    )
  )

let compare = compare_gen Bool.compare

let of_function (f : Sandbox_mode.t -> _) = {
  none = f None;
  symlink = f (Some Symlink);
  copy = f (Some Copy);
}

let no_special_requirements = of_function (fun _ -> true)

let no_sandboxing =
  of_function Option.is_none

let needs_sandboxing =
  of_function Option.is_some

let default = no_special_requirements

let user_rule = no_sandboxing

type conflict = Conflict


module Partial = struct
  type t = bool option gen

  let get_unique eq l =
    match l with
    | [] -> Ok None
    | x :: xs ->
      if List.for_all xs ~f:(eq x)
      then
        Ok (Some x)
      else
        Error Conflict

  (** [merge] behaves like [inter] when there is no error, but it can
      detect a nonsensical configuration where [inter] can't. *)
  let merge ~loc items =
    let merge_field field_name field =
      match
        get_unique Bool.equal
          (List.filter_map items ~f:field)
      with
      | Error Conflict ->
        User_error.raise ~loc [ Pp.text (
          sprintf "Inconsistent sandboxing configuration. Sandboxing mode \
                   %s is both allowed and disallowed" field_name)
        ]
      | Ok None ->
        (* allowed if not forbidden *)
        true
      | Ok (Some v) -> v
    in
    let none = merge_field "none" (fun t -> t.none) in
    let symlink = merge_field "symlink" (fun t -> t.symlink) in
    let copy = merge_field "copy" (fun t -> t.copy) in
    { none; symlink; copy }

  let no_information = { none = None; symlink = None; copy = None }

  let no_special_requirements = {
    none = Some true;
    symlink = Some true;
    copy = Some true;
  }

  let no_sandboxing = {
    none = Some true;
    symlink = Some false;
    copy = Some false;
  }

  let needs_sandboxing = { no_information with none = Some false; }

  let disallow (mode : Sandbox_mode.t) =
    match mode with
    | None ->
      { no_information with none = Some false }
    | Some Symlink ->
      { no_information with symlink = Some false }
    | Some Copy ->
      { no_information with copy = Some false }
end

let disallow (t : Sandbox_mode.t) = match t with
  | None ->
    { no_special_requirements with none = false }
  | Some Copy ->
    { no_special_requirements with copy = false }
  | Some Symlink ->
    { no_special_requirements with symlink = false }

let inter x y = {
  none = x.none && y.none;
  copy = x.copy && y.copy;
  symlink = x.symlink && y.symlink;
}

let mem t (mode : Sandbox_mode.t) = match mode with
  | None -> t.none
  | Some Copy -> t.copy
  | Some Symlink -> t.symlink

let equal x y = match compare x y with
  | Eq -> true
  | Lt | Gt -> false
