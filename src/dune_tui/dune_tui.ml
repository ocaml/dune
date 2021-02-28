open Stdune

module Event = struct
  type t = Quit
end

type t = { backend : (module Console.Backend.S) }

let backend t = t.backend

let create ~on_event =
  let term = Notty_unix.Term.create () in
  let read_events () =
    match Notty_unix.Term.event term with
    | `Key (`ASCII 'Q', [ `Ctrl ])
    | `Key (`Escape, []) ->
      on_event Event.Quit
    | _ -> ()
  in
  let (_ : Thread.t) = Thread.create read_events () in
  let module Backend = struct
    let messages = Queue.create ()

    let status = ref None

    let reset () = ()

    let to_image () =
      let messages =
        Queue.to_list messages
        |> List.map ~f:(fun pp ->
               let pp = User_message.pp pp in
               Notty.I.string (Format.asprintf "%a" Pp.to_fmt pp))
      in
      let all =
        match !status with
        | None -> messages
        | Some s ->
          let s = Format.asprintf "%a" Pp.to_fmt s in
          Notty.I.string s :: messages
      in
      Notty.I.vcat all

    let update () = Notty_unix.Term.image term (to_image ())

    let print_user_message (m : User_message.t) : unit =
      Queue.push messages m;
      update ()

    let set_status_line s =
      status := s;
      update ()
  end in
  { backend = (module Backend) }
