open Stdune
module Console = Dune_console

(** This script generates a changelog entry for a PR. It asks the user for information
    about the pull request and then generates a file in the [changes_directory]. *)

let changes_directory = Path.source (Path.Source.of_string "doc/changes")
let username_file = Path.append_local changes_directory (Path.Local.of_string "username")

let () =
  let description =
    Console.printf "What is the change about?";
    read_line ()
  in
  let pr_number =
    Console.printf "PR number? (Leave blank if you don't know)";
    match read_line () with
    | "" -> "????"
    | s -> s
  in
  let username =
    match Path.stat username_file with
    | Ok { st_kind = Unix.S_REG; _ } ->
      Console.printf "Using username from %s." (Path.to_string_maybe_quoted username_file);
      let raw_username = Io.read_file username_file in
      String.trim raw_username
    | _ ->
      Console.printf "What is your username? (This will be saved for later)";
      let username = read_line () in
      Path.mkdir_p changes_directory;
      Io.write_file username_file username;
      username
  in
  let issues =
    Console.printf "Which issue numbers does this change fix?";
    match read_line () with
    | "" -> ""
    | s ->
      String.split ~on:' ' s
      |> List.map ~f:(fun s -> "#" ^ s)
      |> String.enumerate_and
      |> fun x -> ", fixes " ^ x
  in
  let pp =
    Pp.concat
      [ Pp.verbatim "- "
      ; Pp.box (Pp.textf "%s (#%s%s, @%s)" description pr_number issues username)
      ; Pp.newline
      ]
  in
  let entry_file =
    Path.append_source changes_directory (Path.Source.of_string (pr_number ^ ".md"))
  in
  if Path.exists entry_file
  then (
    Console.print_user_message
      (User_error.make
         [ Pp.textf "File %s already exists." (Path.to_string_maybe_quoted entry_file) ]);
    exit 1)
  else Console.printf "Changelog entry in: %s" (Path.to_string_maybe_quoted entry_file);
  Io.write_lines entry_file [ Format.asprintf "%a" Pp.to_fmt pp ]
;;
