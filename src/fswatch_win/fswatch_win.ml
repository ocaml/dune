module Event = struct
  type action =
    | Added
    | Removed
    | Modified
    | Renamed_old
    | Renamed_new

  type t =
    { directory : string
    ; path : string
    ; action : action
    }

  let directory t = t.directory
  let path t = t.path
  let action t = t.action

  let dyn_of_action = function
    | Added -> Dyn.variant "Added" []
    | Removed -> Dyn.variant "Removed" []
    | Modified -> Dyn.variant "Modified" []
    | Renamed_old -> Dyn.variant "Renamed_old" []
    | Renamed_new -> Dyn.variant "Renamed_new" []
  ;;

  let to_dyn t =
    Dyn.record
      [ "directory", Dyn.string t.directory
      ; "path", Dyn.String t.path
      ; "action", dyn_of_action t.action
      ]
  ;;
end

type t

external create : unit -> t = "fswatch_win_create"
external wait : t -> sleep:int -> Event.t list = "fswatch_win_wait"
external add : t -> string -> unit = "fswatch_win_add"

let add t p =
  if Filename.is_relative p then invalid_arg "Fswatch_win.add";
  add t p
;;

let wait t ~sleep =
  List.filter
    (function
      | { Event.action = Modified; path; directory } ->
        (try not (Sys.is_directory (Filename.concat directory path)) with
         | Sys_error _ -> (* should not happen *) true)
      | _ -> true)
    (wait t ~sleep)
;;

external shutdown : t -> unit = "fswatch_win_shutdown"
