open Import
module Display = Dune_engine.Display

let depexts_hint = function
  | [] -> None
  | depexts ->
    [ Pp.textf "You may want to verify the following depexts are installed:"
    ; Pp.enumerate ~f:Pp.verbatim depexts
    ]
    |> Pp.concat_map ~sep:Pp.cut ~f:(fun pp -> Pp.box pp)
    |> Option.some
;;

module Output : sig
  type error

  val io : error -> Process.Io.output Process.Io.t

  val with_error
    :  accepted_exit_codes:int Predicate.t
    -> pkg:Dune_pkg.Package_name.t * Loc.t
    -> depexts:string list
    -> display:Display.t
    -> (error -> 'a)
    -> 'a

  val prerr : rc:int -> error -> unit
end = struct
  type error =
    { pkg : Dune_pkg.Package_name.t * Loc.t
    ; depexts : string list
    ; filename : Dpath.t
    ; io : Process.Io.output Process.Io.t
    ; accepted_exit_codes : int Predicate.t
    ; display : Display.t
    }

  let io t = t.io

  let with_error ~accepted_exit_codes ~pkg ~depexts ~display f =
    let filename = Temp.create File ~prefix:"dune-pkg" ~suffix:"stderr" in
    let io = Process.Io.(file filename Out) in
    let t = { filename; io; accepted_exit_codes; display; pkg; depexts } in
    let result = f t in
    Temp.destroy File filename;
    result
  ;;

  let to_paragraphs t error =
    let pp_pkg = Pp.textf "Logs for package %s" (Package.Name.to_string (fst t.pkg)) in
    [ pp_pkg; Pp.verbatim error ]
  ;;

  let prerr ~rc error =
    let hints =
      lazy
        (match depexts_hint error.depexts with
         | None -> []
         | Some h -> [ h ])
    in
    let loc = snd error.pkg in
    match Predicate.test error.accepted_exit_codes rc, error.display with
    | false, _ ->
      let paragraphs = Stdune.Io.read_file error.filename |> to_paragraphs error in
      User_warning.emit ~hints:(Lazy.force hints) ~loc ~is_error:true paragraphs
    | true, Display.Verbose ->
      let content = Stdune.Io.read_file error.filename in
      if not (String.is_empty content)
      then (
        let paragraphs = to_paragraphs error content in
        User_warning.emit ~hints:(Lazy.force hints) ~loc paragraphs)
    | true, _ -> ()
  ;;
end

module Windows_executables = struct
  (* Parse a shebang line (without the #! prefix) and return the executable name
   and its arguments. *)
  let parse_shebang_line line =
    let line = String.trim line in
    let parts = String.split_on_char ~sep:' ' line |> List.filter ~f:(fun x -> x <> "") in
    match parts with
    | [] -> None, [] (* empty shebang line *)
    | executable_path :: exe_args ->
      let exe_name = Filename.basename executable_path in
      let exe, extra_args =
        if exe_name <> "env"
        then
          ( Some exe_name
          , if List.is_empty exe_args then [] else [ String.concat ~sep:" " exe_args ] )
          (* shebang lines only support a single argument *)
        else (
          (* Currently, we only support the following forms of env shebang lines:
             #!/usr/bin/env prog_name
             #!/usr/bin/env -S prog_name arg1 arg2 ...
          *)
          match exe_args with
          | [] -> None, [] (* env command with no args *)
          | "-S" :: [] -> None, [] (* env -S with no args *)
          | "-S" :: name :: cmd_args ->
            if String.starts_with name ~prefix:"-"
            then None, [] (* Don't support env -S --other-args prog_name ... *)
            else Some name, cmd_args (* env -S allows multiple args *)
          | name :: [] -> Some name, [] (* env with a single prog_name arg *)
          | _name :: _ -> None, [] (* env with multiple args *))
      in
      exe, extra_args
  ;;

  (* This function adds support for shebang line parsing on Windows to allow
     running scripts in run actions using the correct executable. *)
  let adjust_prog_and_args ~metadata ~path ?dir prog args =
    let { Process.loc; annots; _ } = metadata in
    let prog_name = Path.basename prog in
    let parse_win32_prog_and_args prog =
      Stdune.Io.with_file_in ~binary:true prog ~f:(fun ic ->
        match really_input_string ic 2 <> "#!" with
        | true -> None, []
        | false ->
          (* CR-someday: Limit how many bytes we read here? *)
          let line = input_line ic in
          let prog, args = parse_shebang_line line in
          (match prog with
           | None ->
             User_error.raise
               ?loc
               ~annots
               [ Pp.textf "Dune could not parse the shebang line in %s:" prog_name
               ; Pp.textf "  #!%s" line
               ; Pp.textf
                   "Dune currently only supports the following forms of shebang lines:"
               ; Pp.textf "  #!/path/to/executable <argument>"
               ; Pp.textf "  #!/path/to/env <executable>"
               ; Pp.textf "  #!/path/to/env -S <executable> <arg1> <arg2> ..."
               ; Pp.textf
                   "If this is a valid shebang line that should be parsed by Dune, \
                    please report upstream including the contents of the shebang line."
               ]
           | _ -> prog, args))
    in
    match parse_win32_prog_and_args prog with
    | (exception Sys_error _) | (exception End_of_file) -> prog, args
    | None, _ -> prog, args
    | Some cmd, extra_args ->
      (match Bin.which ~path cmd with
       | None ->
         User_error.raise
           ?loc
           ~annots
           [ Pp.textf "%s: No such file or directory" prog_name ]
       | Some cmd ->
         let prog_str = Path.reach_for_running ?from:dir prog in
         let args = List.concat [ extra_args; prog_str :: args ] in
         cmd, args)
  ;;
end

module Spec = struct
  type 'path chunk =
    | String of string
    | Path of 'path

  type 'path arg = 'path chunk Array.Immutable.t

  type ('path, 'target) t =
    { prog : ('path, Action.Prog.Not_found.t) result
    ; args : 'path arg Array.Immutable.t
    ; ocamlfind_destdir : 'path
    ; pkg : Dune_pkg.Package_name.t * Loc.t
    ; depexts : string list
    }

  let name = "run-with-path"
  let version = 2

  let map_arg arg ~f =
    Array.Immutable.map arg ~f:(function
      | String _ as s -> s
      | Path p -> Path (f p))
  ;;

  let bimap t f _g =
    { t with
      args = Array.Immutable.map t.args ~f:(map_arg ~f)
    ; ocamlfind_destdir = f t.ocamlfind_destdir
    ; prog = Result.map t.prog ~f
    }
  ;;

  let is_useful_to ~memoize:_ = true

  let encode { prog; args; ocamlfind_destdir; pkg = _; depexts = _ } path _ : Sexp.t =
    let prog : Sexp.t =
      match prog with
      | Ok p -> path p
      | Error e -> Atom e.program
    in
    let args =
      Array.Immutable.to_list_map args ~f:(fun x ->
        Sexp.List
          (Array.Immutable.to_list_map x ~f:(function
             | String s -> Sexp.Atom s
             | Path p -> path p)))
    in
    List [ List ([ prog ] @ args); path ocamlfind_destdir ]
  ;;

  let action
        { prog; args; ocamlfind_destdir; pkg; depexts }
        ~(ectx : Action.context)
        ~(eenv : Action.env)
    =
    let open Fiber.O in
    let display = !Clflags.display in
    match prog with
    | Error e -> Action.Prog.Not_found.raise e
    | Ok prog ->
      let args =
        Array.Immutable.to_list_map args ~f:(fun arg ->
          Array.Immutable.to_list_map arg ~f:(function
            | String s -> s
            | Path p -> Path.to_absolute_filename p)
          |> String.concat ~sep:"")
      in
      let metadata = Process.create_metadata ~purpose:ectx.metadata.purpose () in
      let dune_folder =
        let bin_folder = Temp.create Dir ~prefix:"dune" ~suffix:"self-in-path" in
        let src = Path.of_string Sys.executable_name in
        let dst = Path.relative bin_folder "dune" in
        Io.portable_symlink ~src ~dst;
        bin_folder
      in
      let env =
        eenv.env
        |> Env.add
             ~var:"OCAMLFIND_DESTDIR"
             ~value:(Path.to_absolute_filename ocamlfind_destdir)
        |> Env_path.cons ~dir:dune_folder
      in
      let prog, args =
        if not Sys.win32
        then prog, args
        else (
          let path = Env_path.path env in
          Windows_executables.adjust_prog_and_args
            ~metadata
            ~path
            ~dir:eenv.working_dir
            prog
            args)
      in
      Output.with_error
        ~accepted_exit_codes:eenv.exit_codes
        ~pkg
        ~depexts
        ~display
        (fun error ->
           let stdout_to =
             match !Clflags.debug_package_logs, display with
             | true, _ | false, Display.Verbose -> eenv.stdout_to
             | _ -> Process.Io.(null Out)
           in
           let* _, rc =
             Process.run
               Return
               prog
               args
               ~display
               ~metadata
               ~stdout_to
               ~stderr_to:(Output.io error)
               ~stdin_from:eenv.stdin_from
               ~dir:eenv.working_dir
               ~env
           in
           Output.prerr ~rc error;
           Fiber.return ())
  ;;
end

module A = Action_ext.Make (Spec)

let action ~pkg ~depexts prog args ~ocamlfind_destdir =
  A.action { Spec.prog; args; ocamlfind_destdir; pkg; depexts }
;;
