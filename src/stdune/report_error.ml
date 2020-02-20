exception Already_reported

type printer =
  { loc : Loc.t option
  ; pp : Format.formatter -> unit
  ; hint : string option
  ; backtrace : bool
  }

let make_printer ?(backtrace = false) ?hint ?loc pp =
  { loc; pp; hint; backtrace }

let rec tag_handler ppf (style : User_message.Style.t) pp =
  (Format.pp_open_tag ppf
     ( User_message.Print_config.default style
     |> Ansi_color.Style.escape_sequence ) [@warning "-3"]);
  Pp.render ppf pp ~tag_handler;
  (Format.pp_close_tag ppf () [@warning "-3"])

let render ppf pp = Pp.render ppf pp ~tag_handler

let get_printer = function
  | User_error.E msg ->
    { loc = msg.loc
    ; backtrace = false
    ; hint =
        ( match msg.hints with
        | [] -> None
        | hint :: _ ->
          Some (Format.asprintf "@[%a@]" Pp.render_ignore_tags hint) )
    ; pp =
        (fun ppf ->
          render ppf (User_message.pp { msg with loc = None; hints = [] }))
    }
  | Code_error.E t ->
    let maybe_pp_loc fmt =
      Option.iter ~f:(Format.fprintf fmt "@.%a" Loc.print)
    in
    let pp ppf =
      Format.fprintf ppf
        "@{<error>Internal error, please report upstream including the \
         contents of _build/log.@}\n\
         Description:%a%a\n"
        Pp.render_ignore_tags
        (Dyn.pp (Code_error.to_dyn_without_loc t))
        maybe_pp_loc t.loc
    in
    make_printer ~backtrace:true pp
  | Unix.Unix_error (err, func, fname) ->
    let pp ppf =
      Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n" func fname
        (Unix.error_message err)
    in
    make_printer pp
  | exn ->
    let pp ppf =
      let s = Printexc.to_string exn in
      if String.is_prefix s ~prefix:"File \"" then
        Format.fprintf ppf "%s\n" s
      else
        Format.fprintf ppf "@{<error>Error@}: exception %s\n" s
    in
    make_printer ~backtrace:true pp

let i_must_not_segfault =
  let x =
    lazy
      (at_exit (fun () ->
           prerr_endline
             "\n\
              I must not segfault.  Uncertainty is the mind-killer.  \
              Exceptions are\n\
              the little-death that brings total obliteration.  I will fully \
              express\n\
              my cases.  Execution will pass over me and through me.  And when \
              it\n\
              has gone past, I will unwind the stack along its path.  Where the\n\
              cases are handled there will be nothing.  Only I will remain."))
  in
  fun () -> Lazy.force x

let reported = ref Digest.Set.empty

let clear_reported () = reported := Digest.Set.empty

let buf = Buffer.create 128

let ppf = Format.formatter_of_buffer buf

let report ?(extra = fun _ -> None) { Exn_with_backtrace.exn; backtrace } =
  match exn with
  | Already_reported -> ()
  | _ ->
    let p = get_printer exn in
    let loc =
      if Option.equal Loc.equal p.loc (Some Loc.none) then
        None
      else
        p.loc
    in
    Option.iter loc ~f:(Loc.print ppf);
    p.pp ppf;
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    (* Hash to avoid keeping huge errors in memory *)
    let hash = Digest.string s in
    if Digest.Set.mem !reported hash then
      Buffer.clear buf
    else (
      reported := Digest.Set.add !reported hash;
      if p.backtrace || !Clflags.debug_backtraces then
        Format.fprintf ppf "Backtrace:\n%s"
          (Printexc.raw_backtrace_to_string backtrace);
      Option.iter (extra loc) ~f:(Format.fprintf ppf "%a@\n" render);
      Option.iter p.hint ~f:(fun s -> Format.fprintf ppf "Hint: %s\n" s);
      Format.pp_print_flush ppf ();
      let s = Buffer.contents buf in
      Buffer.clear buf;
      Console.print s;
      if p.backtrace then i_must_not_segfault ()
    )
