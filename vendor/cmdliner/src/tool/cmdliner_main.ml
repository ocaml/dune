(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let error_to_failure = function Ok v -> v | Error e -> failwith e

let find_sub ?(start = 0) ~sub s =
  (* naive algorithm, worst case O(length sub * length s) *)
  let len_sub = String.length sub in
  let len_s = String.length s in
  let max_idx_sub = len_sub - 1 in
  let max_idx_s = if len_sub <> 0 then len_s - len_sub else len_s - 1 in
  let rec loop i k =
    if i > max_idx_s then None else
    if k > max_idx_sub then Some i else
    if k > 0 then
      if String.get sub k = String.get s (i + k)
      then loop i (k + 1) else loop (i + 1) 0
    else
    if String.get sub 0 = String.get s i
    then loop i 1 else loop (i + 1) 0
  in
  loop start 0

let rec mkdir dir = (* Can be replaced by Sys.mkdir once we drop OCaml < 4.12 *)
  (* On Windows -p does not exist we do it ourselves on all platforms. *)
  let err_cmd exit cmd =
    raise (Sys_error (strf "exited with %d: %s\n" exit cmd))
  in
  let run_cmd args =
    let cmd = String.concat " " (List.map Filename.quote args) in
    let cmd = if Sys.win32 then strf {|"%s"|} cmd else cmd in
    let exit =  Sys.command cmd in
    if exit = 0 then () else err_cmd exit cmd
  in
  let parent = Filename.dirname dir in
  (if String.equal dir parent then () else mkdir (Filename.dirname dir));
  (if Sys.file_exists dir then () else run_cmd ["mkdir"; dir])

let read_file file =
  (* In_channel is < 4.14 *)
  let read file ic =
    try
      (* This fails on `stdin` or large files on 32-bit. Once we require
         4.14 In_channel.input_all handles these quirks. *)
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      really_input ic buf 0 len; close_in ic;
      Ok (Bytes.unsafe_to_string buf)
    with
    | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdin () = set_binary_mode_in stdin true in
  try match file with
  | "-" -> binary_stdin (); read file stdin
  | file ->
      let ic = open_in_bin file in
      let finally () = close_in_noerr ic in
      Fun.protect ~finally @@ fun () -> read file ic
  with Sys_error e -> Error e

let write_file file s =
  (* Out_channel is < 4.14 *)
  let write file s oc = try Ok (output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = set_binary_mode_out stdout true in
  try match file with
  | "-" -> binary_stdout (); write file s stdout
  | file ->
      let oc = open_out_bin file in
      let finally () = close_out_noerr oc in
      Fun.protect ~finally @@ fun () -> write file s oc
  with Sys_error e -> Error e

let with_binary_stdout f =
  try let () = set_binary_mode_out stdout true in f () with
  | Sys_error e | Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let exec_stdout tool ~args =
  (* The cmd munging logic can be replaced by Filename.quote_command once we
     drop OCaml < 4.10 *)
  let quote_tool tool =
    Filename.quote @@
    if Sys.win32 then String.map (function '/' -> '\\' | c -> c) tool else tool
  in
  try
    let tmp = Filename.temp_file "cmd" "stdout" in
    let tool = quote_tool tool and args = List.map Filename.quote args in
    let cmd = String.concat " " (tool :: args) in
    let exec = String.concat " > " [cmd; Filename.quote tmp] in
    let exec = if Sys.win32 then strf {|"%s"|} exec else exec in
    match Sys.command exec with
    | 0 ->
        let ic = open_in_bin tmp in
        let finally () =
          close_in_noerr ic;
          try Sys.remove tmp with Sys_error _ -> () (* not that important *)
        in
        let len = in_channel_length ic in
        Fun.protect ~finally @@ fun () ->
        let stdout = really_input_string ic len in
        Ok stdout
    | exit -> Error (strf "%s: exited with %d" exec exit)
  with
  | Sys_error e -> Error e

(* Opam .install file updating *)

let update_opam_install_section ~opam_src ~section moves =
  (* Can fail in all sorts of ways if the '$(section):' string appears
     in the file moves of [opam_src] *)
  let move_to_string (src, dst) = Printf.sprintf "  %S {%S}" src dst in
  let open_section ~section opam_src =
    let section = section ^ ":" in
    match find_sub ~sub:section opam_src with
    | None -> (strf "%s\n%s [" opam_src section), " ]"
    | Some start ->
        match String.index_from_opt opam_src start '[' with
        | None ->
            failwith (strf "Could not open section %s in opam file" section)
        | Some i ->
            let j = i + 1 in
            String.sub opam_src 0 j,
            String.sub opam_src j (String.length opam_src - j)
  in
  let before, after = open_section ~section opam_src in
  let moves = List.rev_map move_to_string moves in
  let moves = String.concat "\n" ("" :: moves) in
  String.concat "" [before; moves; after]

let maybe_update_opam_install_file ~update_opam_install section moves =
  match update_opam_install with
  | None -> ()
  | Some "-" -> failwith "- is stdin, it cannot be updated"
  | Some file ->
      let opam_src =
        if not (Sys.file_exists file) then "" else
        read_file file |> error_to_failure
      in
      let src = update_opam_install_section ~opam_src ~section moves in
      write_file file src |> error_to_failure

(* Cmdliner based tool introspection.

   Note this is a bit hackish but does the job. At some point we could
   investigate cleaner protocols with the `--cmdliner` reserved option. *)

let split_toolname toolexec =
  let tool, name = Scanf.sscanf toolexec "%s@:%s" (fun n e -> n, e) in
  let name =
    if name <> "" then name else
    let name = Filename.basename tool in
    match Filename.chop_suffix_opt ~suffix:".exe" tool with
    | None -> name | Some name -> name
  in
  tool, name

let get_tool_commands tool =
  (* We get that by using the completion protocol, see doc/cli.mld  *)
  try
    let subcommands cmd =
      let rec find_subs = function
      | "group" :: "Subcommands" :: lines ->
          let rec subs acc = function
          | "group" :: _ | [] -> acc
          | "item" :: sub :: lines ->
              let sub = if cmd = "" then sub else String.concat " " [cmd; sub]in
              subs (sub :: acc) lines
          | _ :: lines -> subs acc lines
          in
          subs [] lines
      | _ :: lines -> find_subs lines
      | [] -> []
      in
      let subs = if cmd = "" then [] else String.split_on_char ' ' cmd in
      let args = "--__complete" :: (subs @ ["--__complete="]) in
      let comps = exec_stdout tool ~args |> error_to_failure in
      let comps = String.split_on_char '\n' comps in
      match comps with
      | "1" :: comps -> find_subs comps
      | version :: comps ->
          failwith (strf "Unsupported cmdliner completion protocol: %S" version)
      | [] ->
          failwith "Could not parse cmdliner completion protocol"
    in
    let rec loop acc = function
    | cmd :: cmds ->
        let subs = subcommands cmd in
        loop (if cmd <> "" then cmd :: acc else acc) (List.rev_append subs cmds)
    | [] -> List.sort String.compare acc
    in
    Ok (loop [] [""])
  with Failure e -> Error e

let get_tool_command_man tool ~name cmd =
  let man_basename =
    let exec = if cmd = "" then name else String.concat " " [name; cmd] in
    (String.map (function ' ' -> '-' | c -> c) exec)
  in
  let add_section man =
    let rec extract_section = function
    | line :: lines ->
        begin match Scanf.sscanf line ".TH %s %d"  (fun _ n -> n) with
        | n -> Ok (n, man_basename, man)
        | exception Scanf.Scan_failure _ -> extract_section lines
        end
    | [] ->
        Error (strf "%s command: Could not extract section from manual"
                 (tool ^ " " ^ cmd))
    in
    extract_section (String.split_on_char '\n' man)
  in
  let subs = if cmd = "" then [] else String.split_on_char ' ' cmd in
  let args = subs @ ["--help=groff"] in
  match exec_stdout tool ~args with
  | Error _ as e -> e
  | Ok man -> add_section man

let get_tool_manpages tool ~name = match get_tool_commands tool with
| Error _ as e -> e
| Ok cmds ->
    try
      let man cmd = get_tool_command_man tool ~name cmd |> error_to_failure in
      Ok (List.sort compare (List.map man ("" :: cmds)))
    with
    | Failure e -> Error e

(* File path actions *)

let log_action act p = Printf.printf "%s \x1B[1m%s\x1B[0m\n%!" act p

let mkdir ~dry_run p =
  if not (Sys.file_exists p) then begin
    log_action "Creating directory" p;
    if not dry_run then mkdir p
  end

let write_file ~dry_run p contents =
  log_action "Writing" p;
  if not dry_run then begin match write_file p contents with
  | Ok () -> ()
  | Error e -> failwith e
  end

(* Shells completion *)

module type SHELL = sig
  val name : string
  val sharedir : string
  val generic_script_name : string
  val generic_completion : string
  val tool_script_name : toolname:string -> string
  val tool_completion : toolname:string -> standalone:bool -> string
end

type shell = (module SHELL)

module Bash = struct
  let name = "bash"
  let sharedir = "bash-completion/completions"
  let generic_script_name = "_cmdliner_generic"
  let generic_completion =
    Cmdliner_data.bash_generic_completion "_cmdliner_generic"

  let tool_script_name ~toolname = toolname
  let tool_completion ~toolname ~standalone =
    if not standalone then
      strf
{|if ! declare -F _cmdliner_generic > /dev/null; then
  _completion_loader _cmdliner_generic
fi
complete -F _cmdliner_generic %s
|} toolname
    else
    let munge s = String.map (function '-' -> '_' | c -> c) s in
    let fun_name = strf "_%s_cmdliner" (munge toolname) in
    let fun_def = Cmdliner_data.bash_generic_completion fun_name in
    strf
{|
%s
if ! declare -F %s > /dev/null; then
  _completion_loader %s
fi
complete -F %s %s
|} fun_def fun_name fun_name fun_name toolname
end

module Zsh = struct
  let name = "zsh"
  let sharedir = "zsh/site-functions"
  let generic_script_name = "_cmdliner_generic"
  let generic_completion =
    Cmdliner_data.zsh_generic_completion "_cmdliner_generic"

  let tool_script_name ~toolname = "_" ^ toolname
  let tool_completion ~toolname ~standalone =
    if not standalone then
      strf
{|#compdef %s
autoload _cmdliner_generic
_cmdliner_generic
|} toolname
    else
    let munge s = String.map (function '-' -> '_' | c -> c) s in
    let fun_name = strf "_%s_cmdliner" (munge toolname) in
    let fun_def = Cmdliner_data.zsh_generic_completion fun_name in
    strf
{|#compdef %s
%s
%s
|} toolname fun_def fun_name
end

module Pwsh = struct
  let name = "pwsh"
  let sharedir = "powershell"
  let generic_script_name = "cmdliner_generic_completion.ps1"
  let generic_completion =
    Cmdliner_data.pwsh_generic_completion "_cmdliner_generic"

  let tool_script_name ~toolname = strf "%s_completion.ps1" toolname
  let tool_completion ~toolname ~standalone =
    if not standalone then
    strf
{|Register-ArgumentCompleter -Native -CommandName %s -ScriptBlock $Global:_cmdliner_generic
|} toolname
    else
    let munge s = String.map (function '-' -> '_' | c -> c) s in
    let fun_name = strf "_%s_cmdliner" (munge toolname) in
    let fun_def = Cmdliner_data.pwsh_generic_completion fun_name in
    strf
{|
%s

Register-ArgumentCompleter -Native -CommandName %s -ScriptBlock %s
|} fun_def toolname fun_name
end

let shells : shell list = [(module Bash); (module Zsh); (module Pwsh)]

let generic_completion (module Shell : SHELL) =
  with_binary_stdout @@ fun () ->
  print_string Shell.generic_completion;
  Cmdliner.Cmd.Exit.ok

let tool_completion (module Shell : SHELL) ~toolname ~standalone =
  with_binary_stdout @@ fun () ->
  print_string (Shell.tool_completion ~toolname ~standalone);
  Cmdliner.Cmd.Exit.ok

(* Install commands *)

let install_generic_completion ~dry_run ~update_opam_install shells sharedir =
  with_binary_stdout @@ fun () ->
  let install ~dry_run sharedir acc (module Shell : SHELL) =
    let rel_path = Filename.concat Shell.sharedir Shell.generic_script_name in
    let dest = Filename.concat sharedir Shell.sharedir in
    let path = Filename.concat sharedir rel_path in
    mkdir ~dry_run dest;
    write_file ~dry_run path Shell.generic_completion;
    (path, rel_path) :: acc
  in
  try
    let moves = List.fold_left (install ~dry_run sharedir) [] shells in
    maybe_update_opam_install_file ~update_opam_install "share_root" moves;
    Cmdliner.Cmd.Exit.ok
  with Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let install_tool_completion
    ~dry_run ~update_opam_install ~shells ~toolnames ~sharedir
    ~standalone_completion:standalone
  =
  with_binary_stdout @@ fun () ->
  let install ~dry_run ~toolnames sharedir acc (module Shell : SHELL) =
    let write acc toolname =
      let rel_path =
        Filename.concat Shell.sharedir (Shell.tool_script_name ~toolname)
      in
      let path = Filename.concat sharedir rel_path  in
      write_file ~dry_run path (Shell.tool_completion ~toolname ~standalone);
      (path, rel_path) :: acc
    in
    mkdir ~dry_run (Filename.concat sharedir Shell.sharedir);
    List.fold_left write acc toolnames
  in
  let moves = List.fold_left (install ~dry_run ~toolnames sharedir) [] shells in
  try
    maybe_update_opam_install_file ~update_opam_install "share_root" moves;
    Cmdliner.Cmd.Exit.ok
  with Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let install_tool_manpages ~dry_run ~update_opam_install ~tools ~mandir =
  (* Note this correctly handles manpages sections but at the moment
     all manpages for tool and commands are in section 1. *)
  let rec get_mans tool =
    let tool, name = split_toolname tool in
    get_tool_manpages tool ~name |> error_to_failure
  in
  try
    let mans = List.sort compare (List.concat (List.map get_mans tools)) in
    let rec install ~dry_run ~last_sec acc = function
    | (sec, basename, man) :: mans ->
        let secdir = strf "man%d" sec in
        let rel_path = Filename.concat secdir (strf "%s.%d" basename sec) in
        let path = Filename.concat mandir rel_path in
        if last_sec <> sec then mkdir ~dry_run (Filename.concat mandir secdir);
        write_file ~dry_run path man;
        install ~dry_run ~last_sec:sec ((path, rel_path) :: acc) mans
    | [] -> acc
    in
    let moves = install ~dry_run ~last_sec:(-1) [] mans in
    maybe_update_opam_install_file ~update_opam_install "man" moves;
    Cmdliner.Cmd.Exit.ok
  with Failure e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

let install_tool_support
    ~dry_run ~update_opam_install tools shells ~prefix ~sharedir ~mandir
    ~standalone_completion
  =
  let sharedir = match sharedir with
  | None -> Filename.concat prefix "share" | Some sharedir -> sharedir
  in
  let mandir = match mandir with
  | None -> Filename.concat sharedir "man" | Some mandir -> mandir
  in
  let rc = install_tool_manpages ~dry_run ~update_opam_install ~tools ~mandir in
  if rc <> Cmdliner.Cmd.Exit.ok then rc else
  let toolnames = List.map snd (List.map split_toolname tools) in
  install_tool_completion
    ~dry_run ~update_opam_install ~shells ~toolnames ~sharedir
    ~standalone_completion

(* Tool command listing command *)

let tool_commands tool = match get_tool_commands tool with
| Ok subs -> List.iter print_endline subs; Cmdliner.Cmd.Exit.ok
| Error e -> prerr_endline e; Cmdliner.Cmd.Exit.some_error

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dry_run =
  let doc = "Do not install, output paths that would be written." in
  Arg.(value & flag & info ["dry-run"] ~doc)

(* Since the program uses functions like Filename.basename we need to
   make sure the given filepaths have the right separator. This may
   not be the case e.g. in package manager build instructions. *)

let path_to_platform_path s =
  if Sys.win32
  then String.map (function '/' -> '\\' | c -> c) s
  else String.map (function '\\' -> '/' | c -> c) s

let platform_dirpath =
  let parser s = Ok (path_to_platform_path s) in
  Arg.Conv.of_conv ~parser Arg.dirpath

let platform_filepath =
  let parser s = Ok (path_to_platform_path s) in
  Arg.Conv.of_conv ~parser Arg.filepath

let standalone_completion =
  let doc =
    "Generate standalone completion scripts. These scripts do \
     not depend on the generic cmdliner completion scripts. For some \
     shells this may result in slower completion."
  in
  Arg.(value & flag & info ["standalone-completion"] ~doc)

let update_opam_install =
  let doc =
    "Update or create an opam $(b,.install) file $(docv) with install moves \
     from the installed files to the corresponding opam install sections. \
     Also performed if $(b,--dry-run) is specified."
  in
  Arg.(value & opt (some platform_filepath) None &
       info ["update-opam-install"] ~doc ~docv:"PKG.install")

let prefix =
  let doc = "$(docv) is the install prefix. For example $(b,/usr/local)." in
  Arg.(required & pos ~rev:true 0 (some platform_dirpath) None &
       info [] ~doc ~docv:"PREFIX")

let sharedir_doc = "$(docv) is the $(b,share) directory to install to."
let sharedir_docv = "SHAREDIR"
let sharedir_posn ~rev n =
  Arg.(required & pos ~rev n (some platform_dirpath) None &
       info [] ~doc:sharedir_doc ~docv:sharedir_docv)

let sharedir_pos0 = sharedir_posn ~rev:false 0
let sharedir_poslast = sharedir_posn ~rev:true 0
let sharedir_opt =
  let absent = "$(i,PREFIX)$(b,/share)" in
  Arg.(value & opt (some platform_dirpath) None &
       info ["sharedir"] ~doc:sharedir_doc ~docv:sharedir_docv ~absent)

let mandir_doc = "$(docv) is the root $(b,man) directory to install to."
let mandir_docv = "MANDIR"
let mandir_poslast =
  Arg.(required & pos ~rev:true 0 (some platform_dirpath) None &
       info [] ~doc:mandir_doc ~docv:mandir_docv)

let mandir_opt =
  let absent = "$(i,SHAREDIR)$(b,/man)" in
  Arg.(value & opt (some platform_dirpath) None &
       info ["mandir"] ~doc:mandir_doc ~docv:mandir_docv ~absent)

let shell_assoc = List.map (fun ((module S : SHELL) as s) -> S.name, s) shells
let shells_doc = Arg.doc_alts_enum shell_assoc
let shell_conv = Arg.enum ~docv:"SHELL" shell_assoc
let shell_doc = strf "$(docv) the shell to support, must be %s." shells_doc
let shells_opt =
  let doc = shell_doc ^ " Repeatable." in
  let absent = "All supported shells" in
  Arg.(value & opt_all shell_conv shells & info ["s"; "shell"] ~absent ~doc)

let shell_posn n =
  Arg.(required & pos n (some shell_conv) None & info [] ~doc:shell_doc)

let shell_pos0 = shell_posn 0
let shell_pos1 = shell_posn 1

let toolname_posn n =
  let doc = "$(docv) is the name of the tool to complete." in
  Arg.(required & pos n (some platform_filepath) None &
       info [] ~doc ~docv:"TOOLNAME")

let toolname_pos0 = toolname_posn 0
let toolname_pos1 = toolname_posn 1
let toolnames_posleft =
  let doc = "$(docv) is the name of the tool to complete. Repeatable." in
  Arg.(non_empty & pos_left ~rev:true 0 string [] &
       info [] ~doc ~docv:"TOOLNAME")

let tools_posleft =
  let doc =
    "$(i,TOOLEXEC) is the tool executable. Searched in the $(b,PATH) unless \
     an explicit file path is specified. $(i,NAME) is the tool name, if \
     unspecified derived from $(i,TOOLEXEC) by taking the basename and \
     stripping any $(b,.exe) extension. Repeatable."
  in
  let docv = "TOOLEXEC[:NAME]" in
  Arg.(non_empty & pos_left ~rev:true 0 platform_filepath [] &
       info [] ~doc ~docv)

let generic_completion_cmd =
  let doc = "Output generic completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the generic cmdliner completion script for a given \
          shell. Examples:";
      `Pre "$(cmd) $(b,zsh)"; `Noblank;
      `Pre "$(b,eval) $(b,\\$\\()$(cmd) $(b,zsh\\))";
      `P "The script needs to be loaded in a shell for tool specific \
          scripts output by the command $(b,tool-completion) to work. See \
          command $(b,install generic-completion) to install them.";
    ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  let+ shell = shell_pos0 in
  generic_completion shell

let tool_commands_cmd =
  let doc = "Output all subcommands of a cmdliner tool" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs all the subcommands of a given cmdliner based \
          tool, one per line. Examples:";
      `Pre "$(cmd) $(b,./mytool)"; `Noblank;
      `Pre "$(cmd) $(b,cmdliner)";
    ]
  in
  Cmd.make (Cmd.info "tool-commands" ~doc ~man) @@
  let+ tool =
    let doc =
      "$(docv) is the tool executable. Searched in the $(b,PATH) unless \
       an explicit file path is specified."
    in
    Arg.(required & pos 0 (some platform_filepath) None &
         info [] ~doc ~docv:"TOOLEXEC")
  in
  tool_commands tool

let tool_completion_cmd =
  let doc = "Output tool completion scripts" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) outputs the tool specific completion script of a given shell. \
          Example:";
      `Pre "$(cmd) $(b,zsh mytool)";
      `P "Note that tool specific completion script need the corresponding \
          generic completion script output by $(b,generic-completion) to be \
          loaded in the shell. To install these scripts see command \
          $(b,install tool-completion).";
    ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  let+ shell = shell_pos0 and+ toolname = toolname_pos1
  and+ standalone = standalone_completion in
  tool_completion shell ~toolname ~standalone

let install_generic_completion_cmd =
  let doc = "Install generic completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs the generic completion script of given shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Examples:";
    `Pre "$(cmd) $(b,/usr/local/share) # All supported shells"; `Noblank;
    `Pre "$(cmd) $(b,--shell zsh /usr/local/share)";
    `P "To inspect the actual scripts use the command \
        $(b,generic-completion).";
  ]
  in
  Cmd.make (Cmd.info "generic-completion" ~doc ~man) @@
  (* No let punning in < 4.13 *)
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ update_opam_install = update_opam_install
  and+ sharedir_pos0 = sharedir_pos0 in
  install_generic_completion ~dry_run ~update_opam_install shells sharedir_pos0

let install_tool_completion_cmd =
  let doc = "Install tool completion scripts" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs tool completion script of given tools and shells in \
        a $(b,share) directory according to specific shell conventions. \
        Directories are created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,mytool) $(b,/usr/local/share)  # All supported shells";
    `Noblank;
    `Pre "$(cmd) $(b,--shell zsh mytool /usr/local/share)";
    `P "Note that the command $(b,install tool-support) also installs \
        completions like this command does. To inspect the actual scripts \
        use the command $(b,tool-completion).";
  ]
  in
  Cmd.make (Cmd.info "tool-completion" ~doc ~man) @@
  (* No let punning in < 4.13 *)
  let+ dry_run = dry_run and+ shells = shells_opt
  and+ update_opam_install = update_opam_install
  and+ toolnames = toolnames_posleft and+ sharedir = sharedir_poslast
  and+ standalone_completion = standalone_completion in
  install_tool_completion
    ~dry_run ~update_opam_install ~shells ~toolnames ~sharedir
    ~standalone_completion

let install_tool_manpages_cmd =
  let doc = "Install tool and subcommand manpages" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) installs the manpages of the tool and its commands \
        according in directories of a $(b,man) directory. Directories are \
        created if needed. \
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,./mytool) $(b,/usr/local/share/man)";
    `P "Note that the command $(b,install tool-support) also installs manpages \
        like this command does."
  ]
  in
  Cmd.make (Cmd.info "tool-manpages" ~doc ~man) @@
  let+ dry_run = dry_run and+ update_opam_install = update_opam_install
  and+ tools = tools_posleft and+ mandir = mandir_poslast in
  install_tool_manpages ~dry_run ~update_opam_install ~tools ~mandir

let install_tool_support_cmd =
  let doc = "Install both tool completion and manpages" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) combines commands $(b,install tool-completion) and \
        $(b,install tool-manpages) to install all tool support files \
        in a given $(i,PREFIX) which is assumed to follow the Filesystem \
        Hierarchy Standard.
        Use options $(b,--sharedir) and/or $(b,--mandir) if that is
        not the case (e.g. in $(b,opam) as of writing).
        Use option $(b,--dry-run) to see which paths would be written. \
        Example:";
    `Pre "$(cmd) $(b,./mytool /usr/local)"; `Noblank;
    `Pre "$(cmd) $(b,--update-opam-install=mypkg.install) \\\\ \n\
          \         $(b,_build/mytool _build/prefix)";
  ]
  in
  Cmd.make (Cmd.info "tool-support" ~doc ~man) @@
  let+ dry_run = dry_run and+ update_opam_install = update_opam_install
  and+ shells = shells_opt and+ tools = tools_posleft
  and+ sharedir = sharedir_opt and+ mandir = mandir_opt and+ prefix = prefix
  and+ standalone_completion = standalone_completion in
  install_tool_support
    ~dry_run ~update_opam_install tools shells ~prefix ~sharedir ~mandir
    ~standalone_completion

let install_cmd =
  let doc = "Install support files for cmdliner tools" in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) subcommands install cmdliner support files. \
          See the library documentation or invoke \
          subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "install" ~doc ~man) @@
  [install_generic_completion_cmd; install_tool_completion_cmd;
   install_tool_manpages_cmd; install_tool_support_cmd]

let cmd =
  let doc = "Helper tool for cmdliner based tools" in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let man =
    [ `S Manpage.s_description;
      `P "$(tool) is a helper for tools using the cmdliner command line \
          interface library. It helps with installing command line \
          completion scripts and manpages. See the library documentation or \
          invoke subcommands with $(b,--help) for more details."; ]
  in
  Cmd.group (Cmd.info "cmdliner" ~version:"%%VERSION%%" ~doc ~man) ~default @@
  [generic_completion_cmd; tool_commands_cmd; tool_completion_cmd; install_cmd]

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
