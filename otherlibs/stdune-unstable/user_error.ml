module Annot = struct
  type t = ..

  module type S = sig
    type payload

    val make : payload -> t

    val check : t -> (payload -> 'a) -> (unit -> 'a) -> 'a
  end

  module Make (M : sig
    type payload
  end) : S with type payload = M.payload = struct
    type payload = M.payload

    type t += A of M.payload

    let make t = A t

    let check t on_match on_failure =
      match t with
      | A t -> on_match t
      | _ -> on_failure ()
  end
end

exception E of User_message.t * Annot.t option

let prefix =
  Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints paragraphs =
  User_message.make ?loc ?hints paragraphs ~prefix

let raise ?loc ?hints ?annot paragraphs =
  raise (E (make ?loc ?hints paragraphs, annot))

let () =
  Printexc.register_printer (function
    | E (t, _) -> Some (Format.asprintf "%a@?" Pp.to_fmt (User_message.pp t))
    | _ -> None)
