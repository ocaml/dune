open Import

let map_fname = ref (fun x -> x)

type printer =
  { loc       : Loc.t option
  ; pp        : Format.formatter -> unit
  ; hint      : string option
  ; backtrace : bool
  }

let p =
  { loc       = None
  ; pp        = ignore
  ; hint      = None
  ; backtrace = false
  }

let reporters = ref []
let register f = reporters := f :: !reporters

let i_must_not_segfault =
  let x = lazy (at_exit (fun () ->
    prerr_endline "
I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
the little-death that brings total obliteration.  I will fully express
my cases.  Execution will pass over me and through me.  And when it
has gone past, I will unwind the stack along its path.  Where the
cases are handled there will be nothing.  Only I will remain."))
  in
  fun () -> Lazy.force x

(* Firt return value is [true] if the backtrace was printed *)
let report_with_backtrace exn =
  match List.find_map !reporters ~f:(fun f -> f exn) with
  | Some p -> p
  | None ->
    match exn with
    | Exn.Loc_error (loc, msg) ->
      let loc =
        { loc with
          start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
        }
      in
      let pp ppf = Format.fprintf ppf "@{<error>Error@}: %s\n" msg in
      { p with loc = Some loc; pp }
    | Sexp.Of_sexp.Of_sexp (loc, msg, hint') ->
      let loc =
        { loc with
          start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
        }
      in
      let pp ppf = Format.fprintf ppf "@{<error>Error@}: %s%s\n" msg
                     (match hint' with
                      | None -> ""
                      | Some { Sexp.Of_sexp. on; candidates } ->
                        hint on candidates)
      in
      { p with loc = Some loc; pp }
    | Sexp.Parse_error e ->
      let loc = Sexp.Parse_error.loc     e in
      let msg = Sexp.Parse_error.message e in
      let map_pos (pos : Lexing.position) =
        { pos with pos_fname = !map_fname pos.pos_fname }
      in
      let loc : Loc.t =
        { start = map_pos loc.start
        ; stop  = map_pos loc.stop
        }
      in
      { p with
        loc = Some loc
      ; pp  = fun ppf -> Format.fprintf ppf "@{<error>Error@}: %s\n" msg
      }
    | Exn.Fatal_error msg ->
      { p with pp = fun ppf ->
          if msg.[String.length msg - 1] = '\n' then
            Format.fprintf ppf "%s" msg
          else
            Format.fprintf ppf "%s\n" (String.capitalize msg)
      }
    | Stdune.Exn.Code_error sexp ->
      { p with
        backtrace = true
      ; pp = fun ppf ->
          Format.fprintf ppf "@{<error>Internal error, please report upstream \
                              including the contents of _build/log.@}\n\
                              Description:%a\n"
            Usexp.pp_quoted sexp
      }
    | Unix.Unix_error (err, func, fname) ->
      { p with pp = fun ppf ->
          Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n"
            func fname (Unix.error_message err)
      }
    | _ ->
      { p with
        backtrace = true
      ; pp = fun ppf ->
          let s = Printexc.to_string exn in
          if String.is_prefix s ~prefix:"File \"" then
            Format.fprintf ppf "%s\n" s
          else
            Format.fprintf ppf "@{<error>Error@}: exception %s\n" s
      }

let reported = ref String.Set.empty

let report exn =
  let exn, dependency_path = Dep_path.unwrap_exn exn in
  match exn with
  | Already_reported -> ()
  | _ ->
    let backtrace = Printexc.get_raw_backtrace () in
    let ppf = err_ppf in
    let p = report_with_backtrace exn in
    let loc = if p.loc = Some Loc.none then None else p.loc in
    Option.iter loc ~f:(fun loc -> Loc.print ppf loc);
    p.pp ppf;
    Format.pp_print_flush ppf ();
    let s = Buffer.contents err_buf in
    (* Hash to avoid keeping huge errors in memory *)
    let hash = Digest.string s in
    if String.Set.mem !reported hash then
      Buffer.clear err_buf
    else begin
      reported := String.Set.add !reported hash;
      if p.backtrace || !Clflags.debug_backtraces then
        Format.fprintf ppf "Backtrace:\n%s"
          (Printexc.raw_backtrace_to_string backtrace);
      let dependency_path =
        let dependency_path = Option.value dependency_path ~default:[] in
        if !Clflags.debug_dep_path then
          dependency_path
        else
          (* Only keep the part that doesn't come from the build system *)
          let rec drop : Dep_path.Entries.t -> _ = function
            | (Path _ | Alias _) :: l -> drop l
            | l -> l
          in
          match loc with
          | None -> drop dependency_path
          | Some loc ->
            if Filename.is_relative loc.start.pos_fname then
              (* If the error points to a local file, no need to print the
                 dependency stack *)
              []
            else
              drop dependency_path
      in
      if dependency_path <> [] then
        Format.fprintf ppf "%a@\n" Dep_path.Entries.pp
          (List.rev dependency_path);
      Option.iter p.hint ~f:(fun s -> Format.fprintf ppf "Hint: try: %s\n" s);
      Format.pp_print_flush ppf ();
      let s = Buffer.contents err_buf in
      Buffer.clear err_buf;
      print_to_console s;
      if p.backtrace then i_must_not_segfault ()
    end
