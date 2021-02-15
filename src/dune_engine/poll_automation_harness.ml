module Response = struct
  (** Response from automation harness *)
  type t =
    | Exit
    | Files_changed
end

type t = { command : string }

let create ~command = { command }

let build_finished { command } : Response.t =
  match Sys.command command with
  | 0 -> Files_changed
  | _ -> Exit
