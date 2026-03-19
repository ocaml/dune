open Import
open Types

type t =
  { payload : Csexp.t option
  ; message : string
  }

let payload t = t.payload
let message t = t.message

let to_dyn { payload; message } =
  Dyn.record
    [ "message", Dyn.string message; "payload", Dyn.(option Sexp.to_dyn) payload ]
;;

let create ?payload ~message () = { payload; message }

exception E of t

let () =
  Printexc.register_printer (function
    | E { payload; message } ->
      Some
        (let messages =
           match payload with
           | None -> []
           | Some payload -> [ Sexp.pp payload ]
         in
         Format.asprintf "%a@." Pp.to_fmt
         @@ Pp.concat
         @@ (Pp.textf "Version_error: %s" message :: messages))
    | _ -> None)
;;

let to_response_error { payload; message } =
  Response.Error.create ~kind:Invalid_request ?payload ~message ()
;;
