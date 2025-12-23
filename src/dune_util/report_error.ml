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
  { responsible = Developer
  ; msg =
      User_message.make
        ?loc
        [ Pp.tag
            User_message.Style.Error
            (Pp.textf
               "Internal error, please report upstream including the contents of \
                _build/log.")
        ; Pp.text "Description:"
        ; Pp.box ~indent:2 Pp.O.(Pp.verbatim "  " ++ Dyn.pp dyn_without_loc)
        ]
  ; has_embedded_location = false
  ; needs_stack_trace = false
  }
;;

let get_error_from_exn = function
  | Memo.Cycle_error.E raw_cycle ->
    let cycle =
      Memo.Cycle_error.get raw_cycle
      |> List.filter_map ~f:Memo.Stack_frame.human_readable_description
    in
    (match List.last cycle with
     | None ->
       let frames = Memo.Cycle_error.get raw_cycle in
       code_error
         ~loc:None
         ~dyn_without_loc:
           (Dyn.Tuple
              [ String "internal dependency cycle"
              ; Record [ "frames", Dyn.(list Memo.Stack_frame.to_dyn) frames ]
              ])
     | Some last ->
       let first = List.hd cycle in
       let cycle = if last = first then cycle else last :: cycle in
       { responsible = User
       ; msg =
           User_message.make
             ~prefix:User_error.prefix
             [ Pp.text "Dependency cycle between:"; Pp.chain cycle ~f:(fun p -> p) ]
       ; has_embedded_location = false
       ; needs_stack_trace = false
       })
  | User_error.E msg ->
    let has_embedded_location = User_message.has_embedded_location msg in
    let needs_stack_trace = User_message.needs_stack_trace msg in
    let msg =
      match msg.dir with
      | None -> msg
      | Some path ->
        (match Path.extract_build_context (Path.of_string path) with
         | None -> msg
         | Some (ctxt, _) -> { msg with context = Some ctxt })
    in
    { responsible = User; msg; has_embedded_location; needs_stack_trace }
  | Code_error.E e ->
    code_error ~loc:e.loc ~dyn_without_loc:(Code_error.to_dyn_without_loc e)
  | Unix.Unix_error (error, syscall, arg) ->
    { responsible = User
    ; msg =
        User_error.make
          [ Unix_error.Detailed.pp (Unix_error.Detailed.create error ~syscall ~arg) ]
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
    let loc, pp =
      let s = Printexc.to_string exn in
      match
        Scanf.sscanf s "File %S, line %d, characters %d-%d:" (fun a b c d -> a, b, c, d)
      with
      | Error () -> None, Pp.O.(User_error.prefix ++ Pp.textf " exception %s" s)
      | Ok (fname, line, start, stop) ->
        let loc =
          let start : Lexing.position =
            { pos_fname = fname; pos_lnum = line; pos_cnum = start; pos_bol = 0 }
          in
          let stop = { start with pos_cnum = stop } in
          Loc.create ~start ~stop
        in
        Some loc, Pp.text s
    in
    { responsible = Developer
    ; msg = User_message.make ?loc [ pp ]
    ; has_embedded_location = Option.is_some loc
    ; needs_stack_trace = false
    }
;;

let i_must_not_crash =
  let reported = ref false in
  fun () ->
    if !reported
    then []
    else (
      reported := true;
      [ Pp.nop
      ; Pp.text
          "I must not crash.  Uncertainty is the mind-killer. Exceptions are the \
           little-death that brings total obliteration.  I will fully express my cases.  \
           Execution will pass over me and through me.  And when it has gone past, I \
           will unwind the stack along its path.  Where the cases are handled there will \
           be nothing.  Only I will remain."
      ])
;;

let report_backtraces_flag = ref false
let report_backtraces b = report_backtraces_flag := b
let print_memo_stacks = ref false

let format_memo_stack pps =
  match pps with
  | [] -> None
  | _ ->
    Some
      (List.map pps ~f:(fun pp ->
         Pp.O.(Pp.verbatim "-> " ++ Pp.text "required by " ++ pp |> Pp.box ~indent:3))
       |> Pp.concat ~sep:Pp.cut
       |> Pp.vbox)
;;

let gen_report
      { responsible; msg; has_embedded_location; needs_stack_trace }
      memo_stack
      backtrace
  =
  let loc = if msg.loc = Some Loc.none then None else msg.loc in
  let paragraphs =
    List.concat
      [ msg.paragraphs
      ; (if responsible = User && not !report_backtraces_flag
         then []
         else (
           match backtrace with
           | None -> []
           | Some backtrace ->
             Printexc.raw_backtrace_to_string backtrace
             |> String.split_lines
             |> List.map ~f:(fun line -> Pp.box ~indent:2 (Pp.text line))))
      ; (match
           let memo_stack =
             if !print_memo_stacks || needs_stack_trace
             then memo_stack
             else (
               match loc with
               | None -> if has_embedded_location then [] else memo_stack
               | Some loc ->
                 if Filename.is_relative (Loc.start loc).pos_fname
                 then
                   (* If the error points to a local file, we assume that we
                      don't need to explain to the user how we reached this
                      error. *)
                   []
                 else memo_stack)
           in
           format_memo_stack
             (match responsible with
              | User ->
                List.filter_map memo_stack ~f:Memo.Stack_frame.human_readable_description
              | Developer ->
                List.map memo_stack ~f:(fun frame ->
                  Dyn.pp (Memo.Stack_frame.to_dyn frame)))
         with
         | None -> []
         | Some pp -> [ pp ])
      ; (match responsible with
         | User -> []
         | Developer -> i_must_not_crash ())
      ]
  in
  Dune_console.print_user_message { msg with loc; paragraphs }
;;

let gen_report exn backtrace =
  let exn, memo_stack =
    match exn with
    | Memo.Error.E err -> Memo.Error.get err, Memo.Error.stack err
    | _ -> exn, []
  in
  match exn with
  | Already_reported -> ()
  | _ -> gen_report (get_error_from_exn exn) memo_stack backtrace
;;

let report { Exn_with_backtrace.exn; backtrace } = gen_report exn (Some backtrace)
let report_exception exn = gen_report exn None
