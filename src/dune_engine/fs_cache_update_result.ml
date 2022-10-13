open Import

type t =
  | Skipped (* No need to update a given entry because it has no readers *)
  | Updated of { changed : bool }

let combine x y =
  match (x, y) with
  | Skipped, res | res, Skipped -> res
  | Updated { changed = x }, Updated { changed = y } ->
    Updated { changed = x || y }

let empty = Skipped

let to_dyn = function
  | Skipped -> Dyn.Variant ("Skipped", [])
  | Updated { changed } ->
    Dyn.Variant ("Updated", [ Dyn.Record [ ("changed", Dyn.Bool changed) ] ])

let log_update t ~name path =
  if !Clflags.debug_fs_cache then
    Console.print_user_message
      (User_message.make
         [ Pp.hbox
             (Pp.textf "Updating %s cache for %S: %s" name
                (Path.Outside_build_dir.to_string path)
                (Dyn.to_string (to_dyn t)))
         ])
