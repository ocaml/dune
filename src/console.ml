open! Stdune

type status_line_config =
  { message   : User_message.Style.t Pp.t option
  ; show_jobs : bool
  }

module T = struct

  type t = {
    display : Config0.Display.t;
    mutable status_line : Ansi_color.Style.t list Pp.t;
    mutable status_line_len : int;
    mutable gen_status_line : unit -> status_line_config;
  }

  let hide_status_line t =
    if t.status_line_len > 0 then
      Printf.eprintf "\r%*s\r" t.status_line_len ""

  let show_status_line s =
    Ansi_color.prerr s

  let update_status_line t ~running_jobs =
    if t.display = Progress then begin
      match t.gen_status_line () with
      | { message = None; _ } ->
        hide_status_line t;
        flush stderr
      | { message = Some status_line; show_jobs } ->
        let status_line =
          if show_jobs then
            Pp.seq status_line
              (Pp.verbatim (sprintf " (jobs: %u)" running_jobs))
          else
            status_line
        in
        let status_line =
          Pp.map_tags status_line ~f:User_message.Print_config.default
        in
        let status_line_len =
          String.length
            (Format.asprintf "%a" Pp.pp
               (Pp.map_tags status_line ~f:ignore))
        in
        hide_status_line t;
        show_status_line status_line;
        flush stderr;
        t.status_line <- status_line;
        t.status_line_len <- status_line_len
    end

  let print t msg =
    hide_status_line t;
    prerr_string msg;
    show_status_line t.status_line;
    flush stderr

  let print_user_message t ?config ?margin msg =
    hide_status_line t;
    User_message.prerr ?config ?margin msg;
    show_status_line t.status_line;
    flush stderr

  let hide_status_line t =
    hide_status_line t;
    flush stderr

  let set_status_line_generator t f ~running_jobs =
    t.gen_status_line <- f;
    update_status_line t ~running_jobs
end

let t_var = ref None

let init display =
  t_var := Some {
    T.display;
    status_line = Pp.nop;
    status_line_len = 0;
    gen_status_line = (fun () -> { message = None; show_jobs = false; });
  }

let t () =
  Option.value_exn !t_var

let display () = (t ()).display

let get_status_line_generator () = (t ()).gen_status_line
let set_status_line_generator f ~running_jobs =
  T.set_status_line_generator (t ()) f ~running_jobs
let update_status_line ~running_jobs = T.update_status_line (t ()) ~running_jobs
let hide_status_line () = T.hide_status_line (t ())
let print msg =
  match !t_var with
  | None -> Printf.eprintf "%s%!" msg
  | Some t -> T.print t msg
let print_user_message ?config ?margin msg =
  match !t_var with
  | None -> User_message.prerr ?config ?margin msg
  | Some t -> T.print_user_message t ?config ?margin msg
