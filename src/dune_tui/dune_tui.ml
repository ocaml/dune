open Stdune

module Event = struct
  module Public = struct
    type t = Quit
  end

  module Private = struct
    type t =
      | Prev
      | Next
      | Reset
  end

  type t =
    | Public of Public.t
    | Private of Private.t
end

type event = Event.Public.t = Quit

type t = { backend : (module Console.Backend.S) }

let backend t = t.backend

let rec image_of_pp fmt (pp : User_message.Style.t Pp.t) =
  Pp.to_fmt_with_tags fmt pp ~tag_handler:(fun fmt _style t ->
      image_of_pp fmt t)

let image_of_user_message fmt (m : User_message.t) =
  User_message.pp m |> image_of_pp fmt

let create ~on_event =
  let term = Notty_unix.Term.create () in
  let on_event = function
    | Event.Public e -> on_event e
    | Private _ -> ()
  in
  let read_events () =
    let open Event in
    match Notty_unix.Term.event term with
    | `Key (`ASCII 'Q', [ `Ctrl ])
    | `Key (`Escape, []) ->
      on_event (Public Quit)
    | `Key (`ASCII 'R', []) -> on_event (Private Reset)
    | `Key (`Arrow `Up, _) -> on_event (Private Prev)
    | `Key (`Arrow `Down, _) -> on_event (Private Next)
    | _ -> ()
  in
  let (_ : Thread.t) = Thread.create read_events () in
  let module Backend = struct
    (* TODO protect modifications behind mutex *)
    let messages = Queue.create ()

    let status = ref None

    let reset () =
      status := None;
      Queue.clear messages

    let to_image () =
      let messages =
        Queue.to_list messages
        |> List.map ~f:(fun um -> Notty.I.strf "%a" image_of_user_message um)
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
      Format.eprintf "error yo@.%!";
      Queue.push messages m;
      update ()

    let set_status_line s =
      status := s;
      update ()
  end in
  { backend = (module Backend) }
