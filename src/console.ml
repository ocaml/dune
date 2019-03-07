open! Stdune

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

module T = struct

  type t = {
    display : Config0.Display.t;
    mutable status_line : string;
    mutable gen_status_line : unit -> status_line_config;
  }

  let hide_status_line s =
    let len = String.length s in
    if len > 0 then Printf.eprintf "\r%*s\r" len ""

  let show_status_line s =
    prerr_string s

  let update_status_line t ~running_jobs =
    if t.display = Progress then begin
      match t.gen_status_line () with
      | { message = None; _ } ->
        if t.status_line <> "" then begin
          hide_status_line t.status_line;
          flush stderr
        end
      | { message = Some status_line; show_jobs } ->
        let status_line =
          if show_jobs then
            sprintf "%s (jobs: %u)" status_line running_jobs
          else
            status_line
        in
        hide_status_line t.status_line;
        show_status_line   status_line;
        flush stderr;
        t.status_line <- status_line;
    end

  let print t msg =
    let s = t.status_line in
    hide_status_line s;
    prerr_string msg;
    show_status_line s;
    flush stderr

  let hide_status_line t =
    hide_status_line t.status_line;
    flush stderr

  let set_status_line_generator t f ~running_jobs =
    t.gen_status_line <- f;
    update_status_line t ~running_jobs

end

let t_var = ref None

let init display =
  t_var := Some {
    T.display;
    status_line = "";
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
