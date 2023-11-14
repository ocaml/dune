open Import
include Sandbox_mode.Set

let no_special_requirements = of_func (fun _ -> true)
let no_sandboxing = of_func Option.is_none

let needs_sandboxing =
  of_func (function
    | None | Some Patch_back_source_tree -> false
    | Some _ -> true)
;;

let default = no_special_requirements

type conflict = Conflict

module Partial = struct
  type t = bool option Sandbox_mode.Dict.t

  let get_unique eq l =
    match l with
    | [] -> Ok None
    | x :: xs -> if List.for_all xs ~f:(eq x) then Ok (Some x) else Error Conflict
  ;;

  (** [merge] behaves like [inter] when there is no error, but it can detect a
      nonsensical configuration where [inter] can't. *)
  let merge ~loc items =
    let merge_field field =
      match
        get_unique
          Bool.equal
          (List.filter_map items ~f:(fun item -> Sandbox_mode.Dict.get item field))
      with
      | Error Conflict ->
        User_error.raise
          ~loc
          [ Pp.text
              (sprintf
                 "Inconsistent sandboxing configuration. Sandboxing mode %s is both \
                  allowed and disallowed"
                 (Sandbox_mode.to_string field))
          ]
      | Ok None ->
        (* allowed if not forbidden *)
        true
      | Ok (Some v) -> v
    in
    Sandbox_mode.Set.of_func (fun mode -> merge_field mode)
  ;;

  let no_special_requirements = Sandbox_mode.Dict.of_func (fun _ -> Some true)

  let no_sandboxing =
    Sandbox_mode.Dict.of_func (function
      | None -> Some true
      | Some _ -> Some false)
  ;;

  let needs_sandboxing =
    Sandbox_mode.Dict.of_func (function
      | None -> Some false
      | _ -> None)
  ;;

  let disallow (mode : Sandbox_mode.t) =
    Sandbox_mode.Dict.of_func (fun mode' ->
      if Sandbox_mode.equal mode mode' then Some false else None)
  ;;
end

let disallow (mode : Sandbox_mode.t) =
  Sandbox_mode.Set.of_func (fun mode' -> not (Sandbox_mode.equal mode mode'))
;;
