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

(* Firt return value is [true] if the backtrace was printed *)
let report_with_backtrace exn =
  match List.find_map !reporters ~f:(fun f -> f exn) with
  | Some p -> p
  | None ->
    match exn with
    | Loc.Error (loc, msg) ->
      let loc =
        { loc with
          start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
        }
      in
      let pp ppf = Format.fprintf ppf "@{<error>Error@}: %s\n" msg in
      { p with loc = Some loc; pp }
    | Usexp.Parser.Error e ->
      let pos = Usexp.Parser.Error.position e in
      let msg = Usexp.Parser.Error.message e in
      let pos = { pos with pos_fname = !map_fname pos.pos_fname } in
      let loc = { Loc. start = pos; stop = pos } in
      { p with
        loc = Some loc
      ; pp  = fun ppf -> Format.fprintf ppf "@{<error>Error@}: %s\n" msg
      }
    | Fatal_error msg ->
      { p with pp = fun ppf ->
          if msg.[String.length msg - 1] = '\n' then
            Format.fprintf ppf "%s" msg
          else
            Format.fprintf ppf "%s\n" (String.capitalize msg)
      }
    | Code_error msg ->
      { p with
        backtrace = true
      ; pp = fun ppf ->
          Format.fprintf ppf "@{<error>Internal error, please report upstream \
                              including the contents of _build/log.@}\n\
                              Description: %s\n"
            msg
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

let reported = ref String_set.empty

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
    if String_set.mem !reported hash then
      Buffer.clear err_buf
    else begin
      reported := String_set.add !reported hash;
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
      print_to_console s
    end
