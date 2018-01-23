open Import

let map_fname = ref (fun x -> x)

(* Return [true] if the backtrace was printed *)
let report_with_backtrace ppf exn ~backtrace =
  match exn with
  | Loc.Error (loc, msg) ->
    let loc =
      { loc with
        start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
      }
    in
    Format.fprintf ppf "%a@{<error>Error@}: %s\n" Loc.print loc msg;
    false
  | Usexp.Parser.Error e ->
    let pos = Usexp.Parser.Error.position e in
    let msg = Usexp.Parser.Error.message e in
    let pos = { pos with pos_fname = !map_fname pos.pos_fname } in
    let loc = { Loc. start = pos; stop = pos } in
    Format.fprintf ppf "%a@{<error>Error@}: %s\n" Loc.print loc msg;
    false
  | Fatal_error msg ->
    if msg.[String.length msg - 1] = '\n' then
      Format.fprintf ppf "%s" msg
    else
      Format.fprintf ppf "%s\n" (String.capitalize_ascii msg);
    false
  | Findlib.Findlib (Package_not_available { package; required_by; reason }) ->
    Format.fprintf ppf
      "@{<error>Error@}: External library %S %s.\n" package
      (match reason with
       | Not_found -> "not found"
       | Hidden    -> "is hidden"
       | _         -> "is unavailable");
    List.iter required_by ~f:(Format.fprintf ppf "-> required by %a\n"
                                With_required_by.Entry.pp);
    begin match reason with
    | Not_found -> ()
    | Hidden ->
      Format.fprintf ppf
        "External library %S is hidden because its 'exist_if' \
         clause is not satisfied.\n" package
    | Dependencies_unavailable deps ->
      Format.fprintf ppf
        "External library %S is not available because it depends on the \
         following libraries that are not available:\n" package;
      let deps = Findlib.Package_not_available.top_closure deps in
      let longest = List.longest_map deps ~f:(fun na -> na.package) in
      List.iter deps ~f:(fun (na : Findlib.Package_not_available.t) ->
        Format.fprintf ppf "- %-*s -> %a@\n" longest na.package
          Findlib.Package_not_available.explain na.reason)
    end;
    Format.fprintf ppf
      "Hint: try: %s\n"
      (List.map !Clflags.external_lib_deps_hint ~f:quote_for_shell
       |> String.concat ~sep:" ");
    false
  | Findlib.Findlib
      (External_dep_conflicts_with_local_lib
         { package; required_by; required_locally_in; defined_locally_in }) ->
    Format.fprintf ppf
      "@{<error>Error@}: Conflict between internal and external version of library %S:\n\
       - it is defined locally in %s\n\
       - it is required by external library %a\n\
       %s\n\
       This cannot work.\n"
      package
      (Utils.describe_target
         (Utils.jbuild_file_in ~dir:(Path.drop_optional_build_context defined_locally_in)))
      With_required_by.Entry.pp required_by
      (required_locally_in
       |> List.map ~f:(fun x -> "  -> required by " ^
                                With_required_by.Entry.to_string x)
       |> String.concat ~sep:"\n");
    false
  | Code_error msg ->
    let bt = Printexc.raw_backtrace_to_string backtrace in
    Format.fprintf ppf "@{<error>Internal error, please report upstream \
                        including the contents of _build/log.@}\n\
                        Description: %s\n\
                        Backtrace:\n\
                        %s" msg bt;
    true
  | Unix.Unix_error (err, func, fname) ->
    Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n"
      func fname (Unix.error_message err);
    false
  | _ ->
    let s = Printexc.to_string exn in
    let bt = Printexc.raw_backtrace_to_string backtrace in
    if String.is_prefix s ~prefix:"File \"" then
      Format.fprintf ppf "%s\nBacktrace:\n%s" s bt
    else
      Format.fprintf ppf "@{<error>Error@}: exception %s\nBacktrace:\n%s" s bt;
    true

let report_aux ppf ?dependency_path exn =
  let backtrace = Printexc.get_raw_backtrace () in
  let backtrace_printed = report_with_backtrace ppf exn ~backtrace in
  if !Clflags.debug_dep_path then
    Option.iter dependency_path ~f:(fun dep_path ->
      Format.fprintf ppf "Dependency path:\n    %s\n"
        (String.concat ~sep:"\n--> "
           (List.map dep_path ~f:With_required_by.Entry.to_string)));
  if not backtrace_printed && !Clflags.debug_backtraces then
    Format.fprintf ppf "Backtrace:\n%s"
      (Printexc.raw_backtrace_to_string backtrace);
  Format.pp_print_flush ppf ()


let reported = ref String_set.empty

let report exn =
  let exn, dependency_path = With_required_by.unwrap_exn exn in
  match exn with
  | Already_reported -> ()
  | _ ->
    report_aux err_ppf ?dependency_path exn;
    Format.pp_print_flush err_ppf ();
    let s = Buffer.contents err_buf in
    Buffer.clear err_buf;
    (* To avoid keeping huge errors in memory *)
    let hash = Digest.string s in
    if not (String_set.mem hash !reported) then begin
      reported := String_set.add hash !reported;
      prerr_string s;
      flush stderr
    end

