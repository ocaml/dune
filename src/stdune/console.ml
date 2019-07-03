module Display = struct
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  let all =
    [ "progress" , Progress
    ; "verbose"  , Verbose
    ; "short"    , Short
    ; "quiet"    , Quiet
    ]
end

type status_line_config =
  { message   : User_message.Style.t Pp.t option
  ; show_jobs : bool
  }

module T = struct

  type t = {
    display : Display.t;
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
              (Pp.verbatim (Printf.sprintf " (jobs: %u)" running_jobs))
          else
            status_line
        in
        let status_line =
          Pp.map_tags status_line ~f:User_message.Print_config.default
        in
        let status_line_len =
          String.length (Format.asprintf "%a" Pp.render_ignore_tags status_line)
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

  let print_user_message t ?config msg =
    hide_status_line t;
    Option.iter msg.User_message.loc ~f:(Loc.print Format.err_formatter);
    User_message.prerr ?config { msg with loc = None };
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
let print_user_message ?config msg =
  match !t_var with
  | None -> User_message.prerr ?config msg
  | Some t -> T.print_user_message t ?config msg

let () = User_warning.set_reporter print_user_message
