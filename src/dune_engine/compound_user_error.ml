open Stdune

module T : sig
  type t = private
    { main : User_message.t
    ; related : User_message.t list
    }

  val create : main:User_message.t -> related:User_message.t list -> t
end = struct
  type t =
    { main : User_message.t
    ; related : User_message.t list
    }

  let create ~main ~related =
    let () =
      List.iter related ~f:(fun (related : User_message.t) ->
          match related.loc with
          | Some _ -> ()
          | None ->
            Code_error.raise "related messages must have locations"
              [ ("related", String (User_message.to_string related)) ])
    in
    { main; related }
end

module Annot = struct
  include T

  type payload = t

  let to_dyn { main; related } =
    let open Dyn.Encoder in
    record
      [ ("main", string (User_message.to_string main))
      ; ("related", (list string) (List.map related ~f:User_message.to_string))
      ]
end

include Annot
include User_error.Annot.Make (Annot)

let make ~main ~related = make (create ~main ~related)
