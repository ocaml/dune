open Stdune
open Import

exception Already_reported

type who_is_responsible_for_the_error =
  | User
  | Developer

type error =
  { responsible : who_is_responsible_for_the_error
  ; msg : User_message.t
  ; has_embedded_location : bool
  ; needs_stack_trace : bool
  }

let code_error ~loc ~dyn_without_loc =
  let open Pp.O in
  { responsible = Developer
  ; msg =
      User_message.make ?loc
        [ Pp.tag User_message.Style.Error
            (Pp.textf
               "Internal error, please report upstream including the contents \
                of _build/log.")
        ; Pp.text "Description:"
        ; Pp.box ~indent:2 (Pp.verbatim "  " ++ Dyn.pp dyn_without_loc)
        ]
  ; has_embedded_location = false
  ; needs_stack_trace = false
  }

let get_error_from_exn = function
  | Memo.Cycle_error.E raw_cycle -> (
    let cycle =
      Memo.Cycle_error.get raw_cycle
      |> List.filter_map ~f:Memo.Stack_frame.human_readable_description
    in
    match List.last cycle with
    | None ->
      let frames = Memo.Cycle_error.get raw_cycle in
      code_error ~loc:None
        ~dyn_without_loc:
          (Dyn.Tuple
             [ String "internal dependency cycle"
             ; Record [ ("frames", Dyn.(list Memo.Stack_frame.to_dyn) frames) ]
             ])
    | Some last ->
      let first = List.hd cycle in
      let cycle = if last = first then cycle else last :: cycle in
      { responsible = User
      ; msg =
          User_message.make ~prefix:User_error.prefix
            [ Pp.text "Dependency cycle between:"
            ; Pp.chain cycle ~f:(fun p -> p)
            ]
      ; has_embedded_location = false
      ; needs_stack_trace = false
      })
  | User_error.E msg ->
    let has_embedded_location = User_message.has_embedded_location msg in
    let needs_stack_trace = User_message.needs_stack_trace msg in
    { responsible = User; msg; has_embedded_location; needs_stack_trace }
  | Code_error.E e ->
    code_error ~loc:e.loc ~dyn_without_loc:(Code_error.to_dyn_without_loc e)
  | Unix.Unix_error (error, syscall, arg) ->
    { responsible = User
    ; msg =
        User_error.make
          [ Unix_error.Detailed.pp
              (Unix_error.Detailed.create error ~syscall ~arg)
          ]
    ; has_embedded_location = false
    ; needs_stack_trace = false
    }
  | Sys_error msg ->
    { responsible = User
    ; msg = User_error.make [ Pp.text msg ]
    ; has_embedded_location = false
    ; needs_stack_trace = false
    }
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
    { responsible = Developer
    ; msg = User_message.make ?loc [ pp ]
    ; has_embedded_location = Option.is_some loc
    ; needs_stack_trace = false
    }

let i_must_not_crash =
  let reported = ref false in
  fun () ->
    if !reported then []
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
      ])

let report_backtraces_flag = ref false

let report_backtraces b = report_backtraces_flag := b

let print_memo_stacks = ref false

let format_memo_stack pps =
  match pps with
  | [] -> None
  | _ ->
    Some
      (Pp.vbox
         (Pp.concat ~sep:Pp.cut
            (List.map pps ~f:(fun pp ->
                 Pp.box ~indent:3
                   (Pp.seq (Pp.verbatim "-> ")
                      (Pp.seq (Pp.text "required by ") pp))))))

let report { Exn_with_backtrace.exn; backtrace } =
  let exn, memo_stack =
    match exn with
    | Memo.Error.E err -> (Memo.Error.get err, Memo.Error.stack err)
    | _ -> (exn, [])
  in
  match exn with
  | Already_reported -> ()
  | _ ->
    let { responsible; msg; has_embedded_location; needs_stack_trace } =
      get_error_from_exn exn
    in
    let msg =
      if msg.loc = Some Loc.none then { msg with loc = None } else msg
    in
    let append (msg : User_message.t) pp =
      { msg with paragraphs = msg.paragraphs @ pp }
    in
    let msg =
      if responsible = User && not !report_backtraces_flag then msg
      else
        append msg
          (List.map
             (Printexc.raw_backtrace_to_string backtrace |> String.split_lines)
             ~f:(fun line -> Pp.box ~indent:2 (Pp.text line)))
    in
    let memo_stack =
      if !print_memo_stacks || needs_stack_trace then memo_stack
      else
        match msg.loc with
        | None -> if has_embedded_location then [] else memo_stack
        | Some loc ->
          if Filename.is_relative loc.start.pos_fname then
            (* If the error points to a local file, we assume that we don't need
               to explain to the user how we reached this error. *)
            []
          else memo_stack
    in
    let memo_stack =
      match responsible with
      | User ->
        format_memo_stack
          (List.filter_map memo_stack
             ~f:Memo.Stack_frame.human_readable_description)
      | Developer ->
        format_memo_stack
          (List.map memo_stack ~f:(fun frame ->
               Dyn.pp (Memo.Stack_frame.to_dyn frame)))
    in
    let msg =
      match memo_stack with
      | None -> msg
      | Some pp -> append msg [ pp ]
    in
    let msg =
      match responsible with
      | User -> msg
      | Developer -> append msg (i_must_not_crash ())
    in
    Console.print_user_message msg
