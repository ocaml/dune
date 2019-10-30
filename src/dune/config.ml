open! Stdune
open! Import

let local_install_dir =
  let dir = Path.Build.relative Path.Build.root "install" in
  fun ~context ->
    let context = Context_name.to_string context in
    Path.Build.relative dir context

let local_install_bin_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.Build.relative
    (Path.Build.relative (local_install_dir ~context) "lib")
    (Package.Name.to_string package)

let dev_null =
  Path.of_filename_relative_to_initial_cwd
    ( if Sys.win32 then
      "nul"
    else
      "/dev/null" )

let dune_keep_fname = ".dune-keep"

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")

let inside_dune = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let inside_ci = Option.is_some (Env.get Env.initial "CI")

let show_full_command_on_error () =
  inside_dune || inside_ci || !Clflags.always_show_command_line

open Dune_lang.Decoder

(* the configuration file use the same version numbers as dune-project files
   for simplicity *)
let syntax = Stanza.syntax

module Terminal_persistence = struct
  type t =
    | Preserve
    | Clear_on_rebuild

  let all = [ ("preserve", Preserve); ("clear-on-rebuild", Clear_on_rebuild) ]

  let of_string = function
    | "preserve" -> Ok Preserve
    | "clear-on-rebuild" -> Ok Clear_on_rebuild
    | _ ->
      Error
        "invalid terminal-persistence value, must be 'preserve' or \
         'clear-on-rebuild'"

  let to_string = function
    | Preserve -> "preserve"
    | Clear_on_rebuild -> "clear-on-rebuild"

  let decode =
    plain_string (fun ~loc s ->
        match of_string s with
        | Error m -> User_error.raise ~loc [ Pp.text m ]
        | Ok s -> s)
end

module Display = struct
  include Stdune.Console.Display

  let decode = enum all
end

module Concurrency = struct
  type t =
    | Fixed of int
    | Auto

  let error =
    Error "invalid concurrency value, must be 'auto' or a positive number"

  let of_string = function
    | "auto" -> Ok Auto
    | s -> (
      match int_of_string s with
      | exception _ -> error
      | n ->
        if n >= 1 then
          Ok (Fixed n)
        else
          error )

  let decode =
    plain_string (fun ~loc s ->
        match of_string s with
        | Error m -> User_error.raise ~loc [ Pp.text m ]
        | Ok s -> s)

  let to_string = function
    | Auto -> "auto"
    | Fixed n -> string_of_int n
end

module Sandboxing_preference = struct
  type t = Sandbox_mode.t list

  let decode =
    repeat
      (plain_string (fun ~loc s ->
           match Sandbox_mode.of_string s with
           | Error m -> User_error.raise ~loc [ Pp.text m ]
           | Ok s -> s))
end

module type S = sig
  type 'a field

  type t =
    { display : Display.t field
    ; concurrency : Concurrency.t field
    ; terminal_persistence : Terminal_persistence.t field
    ; sandboxing_preference : Sandboxing_preference.t field
    }
end

module rec M : (S with type 'a field = 'a) = M

include M

module rec Partial : (S with type 'a field := 'a option) = Partial

let merge t (partial : Partial.t) =
  let field from_t from_partial = Option.value from_partial ~default:from_t in
  { display = field t.display partial.display
  ; concurrency = field t.concurrency partial.concurrency
  ; terminal_persistence =
      field t.terminal_persistence partial.terminal_persistence
  ; sandboxing_preference =
      field t.sandboxing_preference partial.sandboxing_preference
  }

let default =
  { display =
      ( if inside_dune then
        Quiet
      else
        Progress )
  ; concurrency =
      ( if inside_dune then
        Fixed 1
      else
        Auto )
  ; terminal_persistence = Terminal_persistence.Preserve
  ; sandboxing_preference = []
  }

let decode =
  let+ display = field "display" Display.decode ~default:default.display
  and+ concurrency =
    field "jobs" Concurrency.decode ~default:default.concurrency
  and+ terminal_persistence =
    field "terminal-persistence" Terminal_persistence.decode
      ~default:default.terminal_persistence
  and+ sandboxing_preference =
    field "sandboxing_preference" Sandboxing_preference.decode
      ~default:default.sandboxing_preference
  and+ () = Dune_lang.Versioned_file.no_more_lang in
  { display; concurrency; terminal_persistence; sandboxing_preference }

let decode = fields decode

let user_config_file =
  Path.relative
    (Path.of_filename_relative_to_initial_cwd Xdg.config_dir)
    "dune/config"

include Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let () = Lang.register syntax ()

let load_config_file p = load p ~f:(fun _lang -> decode)

let load_user_config_file () =
  if Path.exists user_config_file then
    load_config_file user_config_file
  else
    default

let adapt_display config ~output_is_a_tty =
  (* Progress isn't meaningful if inside a terminal (or emacs), so reset the
     display to Quiet if the output is getting piped to a file or something. *)
  let config =
    if config.display = Progress && (not output_is_a_tty) && not inside_emacs
    then
      { config with display = Quiet }
    else
      config
  in
  (* Similarly, terminal clearing is meaningless if stderr doesn't support ANSI
     codes, so revert-back to Preserve in that case *)
  if config.terminal_persistence = Clear_on_rebuild && not output_is_a_tty then
    { config with terminal_persistence = Terminal_persistence.Preserve }
  else
    config
