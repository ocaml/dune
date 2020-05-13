open Stdune

exception Already_reported

type who_is_responsible_for_the_error =
  | User
  | Developer

let get_user_message = function
  | User_error.E msg -> (User, msg)
  | Code_error.E e ->
    let open Pp.O in
    ( Developer
    , User_message.make ?loc:e.loc
        [ Pp.tag User_message.Style.Error
            (Pp.textf
               "Internal error, please report upstream including the contents \
                of _build/log.")
        ; Pp.text "Description:"
        ; Pp.box ~indent:2
            (Pp.verbatim "  " ++ Dyn.pp (Code_error.to_dyn_without_loc e))
        ] )
  | Unix.Unix_error (err, func, fname) ->
    ( User
    , User_error.make
        [ Pp.textf "%s: %s: %s" func fname (Unix.error_message err) ] )
  | Sys_error msg -> (User, User_error.make [ Pp.text msg ])
  | exn ->
    let open Pp.O in
    let s = Printexc.to_string exn in
    let loc, pp =
      match
        Scanf.sscanf s "File %S, line %d, characters %d-%d:" (fun a b c d ->
            (a, b, c, d))
      with
      | Error () -> (None, User_error.prefix ++ Pp.textf " exception %s" s)
      | Ok (fname, line, start, stop) ->
        let start : Lexing.position =
          { pos_fname = fname; pos_lnum = line; pos_cnum = start; pos_bol = 0 }
        in
        let stop = { start with pos_cnum = stop } in
        (Some { Loc.start; stop }, Pp.text s)
    in
    (Developer, User_message.make ?loc [ pp ])

let i_must_not_crash =
  let reported = ref false in
  fun () ->
    if !reported then
      []
    else (
      reported := true;
      [ Pp.nop
      ; Pp.text
          "I must not crash.  Uncertainty is the mind-killer. Exceptions are \
           the little-death that brings total obliteration.  I will fully \
           express my cases.  Execution will pass over me and through me.  And \
           when it has gone past, I will unwind the stack along its path.  \
           Where the cases are handled there will be nothing.  Only I will \
           remain."
      ]
    )

let reported = ref Digest.Set.empty

let report_backtraces_flag = ref false

let report_backtraces b = report_backtraces_flag := b

let clear_reported () = reported := Digest.Set.empty

let buf = Buffer.create 128

let ppf = Format.formatter_of_buffer buf

let report ?(extra = fun _ -> None) { Exn_with_backtrace.exn; backtrace } =
  match exn with
  | Already_reported -> ()
  | _ ->
    let who_is_responsible, msg = get_user_message exn in
    let msg =
      if msg.loc = Some Loc.none then
        { msg with loc = None }
      else
        msg
    in
    let hash = Digest.generic msg in
    if not (Digest.Set.mem !reported hash) then (
      reported := Digest.Set.add !reported hash;
      let append (msg : User_message.t) pp =
        { msg with paragraphs = msg.paragraphs @ pp }
      in
      let msg =
        if who_is_responsible = User && not !report_backtraces_flag then
          msg
        else
          append msg
            (List.map
               (Printexc.raw_backtrace_to_string backtrace |> String.split_lines)
               ~f:(fun line -> Pp.box ~indent:2 (Pp.text line)))
      in
      let msg =
        match extra msg.loc with
        | None -> msg
        | Some pp -> append msg [ pp ]
      in
      let msg =
        match who_is_responsible with
        | User -> msg
        | Developer -> append msg (i_must_not_crash ())
      in
      Console.print_user_message msg
    )
