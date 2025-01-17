module Dune_config = struct
  open Stdune
  open Dune_lang.Decoder
  module Display = Display
  module Scheduler = Dune_engine.Scheduler
  module Sandbox_mode = Dune_engine.Sandbox_mode
  module Console = Dune_console
  module Stanza = Dune_lang.Stanza
  module String_with_vars = Dune_lang.String_with_vars
  module Pform = Dune_lang.Pform
  module Log = Dune_util.Log
  module Config = Dune_config.Config
  module Execution_env = Dune_util.Execution_env

  (* the configuration file use the same version numbers as dune-project files for
     simplicity *)
  let syntax = Stanza.syntax

  module Project_defaults = struct
    type t =
      { authors : string list option
      ; maintainers : string list option
      ; maintenance_intent : string list option
      ; license : string list option
      }

    let decode =
      fields
        (let+ authors = field_o "authors" (repeat string)
         and+ maintainers = field_o "maintainers" (repeat string)
         and+ maintenance_intent =
           field_o "maintenance_intent" Dune_lang.Package_info.decode_maintenance_intent
         and+ license = field_o "license" (repeat string) in
         { authors; maintainers; maintenance_intent; license })
    ;;

    let to_dyn t =
      let f = Dyn.(option (list string)) in
      Dyn.record
        [ "authors", f t.authors
        ; "maintainers", f t.maintainers
        ; "maintenance_intent", f t.maintenance_intent
        ; "license", f t.license
        ]
    ;;
  end

  module Terminal_persistence = struct
    type t =
      | Preserve
      | Clear_on_rebuild
      | Clear_on_rebuild_and_flush_history

    let all =
      [ "preserve", Preserve
      ; "clear-on-rebuild", Clear_on_rebuild
      ; "clear-on-rebuild-and-flush-history", Clear_on_rebuild_and_flush_history
      ]
    ;;

    let to_dyn = function
      | Preserve -> Dyn.Variant ("Preserve", [])
      | Clear_on_rebuild -> Dyn.Variant ("Clear_on_rebuild", [])
      | Clear_on_rebuild_and_flush_history ->
        Variant ("Clear_on_rebuild_and_flush_history", [])
    ;;

    let decode = enum all
  end

  module Concurrency = struct
    type t =
      | Fixed of int
      | Auto

    let error = Error "invalid concurrency value, must be 'auto' or a positive number"

    let of_string = function
      | "auto" -> Ok Auto
      | s ->
        (match Int.of_string s with
         | None -> error
         | Some n -> if n >= 1 then Ok (Fixed n) else error)
    ;;

    let decode =
      plain_string (fun ~loc s ->
        match of_string s with
        | Error m -> User_error.raise ~loc [ Pp.text m ]
        | Ok s -> s)
    ;;

    let to_string = function
      | Auto -> "auto"
      | Fixed n -> string_of_int n
    ;;

    let to_dyn = function
      | Auto -> Dyn.Variant ("Auto", [])
      | Fixed n -> Dyn.Variant ("Fixed", [ Int n ])
    ;;
  end

  module Sandboxing_preference = struct
    type t = Sandbox_mode.t list

    let decode : Sandbox_mode.t Dune_sexp.Decoder.t =
      let open Dune_sexp.Decoder in
      enum
        [ "none", None
        ; "symlink", Some Sandbox_mode.Symlink
        ; "copy", Some Copy
        ; "hardlink", Some Hardlink
        ]
    ;;

    let decode = repeat decode
  end

  module Cache = struct
    module Toggle = struct
      type t =
        | Disabled
        | Enabled_except_user_rules
        | Enabled

      let to_string = function
        | Disabled -> "disabled"
        | Enabled_except_user_rules -> "enabled-except-user-rules"
        | Enabled -> "enabled"
      ;;

      let all =
        List.map
          ~f:(fun x -> to_string x, x)
          [ Disabled; Enabled_except_user_rules; Enabled ]
      ;;

      let decode ~check =
        let open Dune_lang.Decoder in
        enum'
          [ to_string Disabled, return Disabled
          ; to_string Enabled, return Enabled
          ; ( to_string Enabled_except_user_rules
            , check (3, 17) >>> return Enabled_except_user_rules )
          ]
      ;;

      let to_dyn = function
        | Disabled -> Dyn.variant "Disabed" []
        | Enabled_except_user_rules -> Dyn.variant "Enabled_except_user_rules" []
        | Enabled -> Dyn.variant "Enabled" []
      ;;
    end

    module Transport_deprecated = struct
      type t =
        | Daemon
        | Direct

      let all = [ "daemon", Daemon; "direct", Direct ]
      let decode = enum all
    end

    module Storage_mode = struct
      type t = Dune_cache_storage.Mode.t option

      let all =
        ("auto", None)
        :: List.map ~f:(fun (name, mode) -> name, Some mode) Dune_cache_storage.Mode.all
      ;;

      let decode = enum all

      let to_string = function
        | None -> "auto"
        | Some mode -> Dune_cache_storage.Mode.to_string mode
      ;;

      let to_dyn = Dyn.option Dune_cache_storage.Mode.to_dyn
    end
  end

  module Action_output_on_success = struct
    include Dune_engine.Execution_parameters.Action_output_on_success

    let decode = enum all
  end

  module type S = sig
    type 'a field

    type t =
      { display : Display.t field
      ; concurrency : Concurrency.t field
      ; terminal_persistence : Terminal_persistence.t field
      ; sandboxing_preference : Sandboxing_preference.t field
      ; cache_enabled : Cache.Toggle.t field
      ; cache_reproducibility_check : Dune_cache.Config.Reproducibility_check.t field
      ; cache_storage_mode : Cache.Storage_mode.t field
      ; action_stdout_on_success : Action_output_on_success.t field
      ; action_stderr_on_success : Action_output_on_success.t field
      ; project_defaults : Project_defaults.t field
      ; experimental : (string * (Loc.t * string)) list field
      }
  end

  module Make_superpose
      (A : S)
      (B : S)
      (C : S)
      (Merge_field : sig
         val merge_field : 'a A.field -> 'a B.field -> 'a C.field
       end) =
  struct
    let field = Merge_field.merge_field

    let superpose (a : A.t) (b : B.t) : C.t =
      { display = field a.display b.display
      ; concurrency = field a.concurrency b.concurrency
      ; terminal_persistence = field a.terminal_persistence b.terminal_persistence
      ; sandboxing_preference = field a.sandboxing_preference b.sandboxing_preference
      ; cache_enabled = field a.cache_enabled b.cache_enabled
      ; cache_reproducibility_check =
          field a.cache_reproducibility_check b.cache_reproducibility_check
      ; cache_storage_mode = field a.cache_storage_mode b.cache_storage_mode
      ; action_stdout_on_success =
          field a.action_stdout_on_success b.action_stdout_on_success
      ; action_stderr_on_success =
          field a.action_stderr_on_success b.action_stderr_on_success
      ; project_defaults = field a.project_defaults b.project_defaults
      ; experimental = field a.experimental b.experimental
      }
    ;;
  end

  module Make_to_dyn
      (M : S)
      (To_dyn : sig
         val field : ('a -> Dyn.t) -> 'a M.field -> Dyn.t
       end) =
  struct
    open To_dyn

    let to_dyn
      { M.display
      ; concurrency
      ; terminal_persistence
      ; sandboxing_preference
      ; cache_enabled
      ; cache_reproducibility_check
      ; cache_storage_mode
      ; action_stdout_on_success
      ; action_stderr_on_success
      ; project_defaults
      ; experimental
      }
      =
      Dyn.record
        [ "display", field Display.to_dyn display
        ; "concurrency", field Concurrency.to_dyn concurrency
        ; "terminal_persistence", field Terminal_persistence.to_dyn terminal_persistence
        ; ( "sandboxing_preference"
          , field (Dyn.list Sandbox_mode.to_dyn) sandboxing_preference )
        ; "cache_enabled", field Cache.Toggle.to_dyn cache_enabled
        ; ( "cache_reproducibility_check"
          , field
              Dune_cache.Config.Reproducibility_check.to_dyn
              cache_reproducibility_check )
        ; "cache_storage_mode", field Cache.Storage_mode.to_dyn cache_storage_mode
        ; ( "action_stdout_on_success"
          , field Action_output_on_success.to_dyn action_stdout_on_success )
        ; ( "action_stderr_on_success"
          , field Action_output_on_success.to_dyn action_stderr_on_success )
        ; "project_defaults", field Project_defaults.to_dyn project_defaults
        ; ( "experimental"
          , field Dyn.(list (pair string (fun (_, v) -> string v))) experimental )
        ]
    ;;
  end

  module rec M : (S with type 'a field = 'a) = M
  include M

  module Partial = struct
    module rec M : (S with type 'a field = 'a option) = M
    include M

    let empty =
      { display = None
      ; concurrency = None
      ; terminal_persistence = None
      ; sandboxing_preference = None
      ; cache_enabled = None
      ; cache_reproducibility_check = None
      ; cache_storage_mode = None
      ; action_stdout_on_success = None
      ; action_stderr_on_success = None
      ; project_defaults = None
      ; experimental = None
      }
    ;;

    include
      Make_superpose (M) (M) (M)
        (struct
          let merge_field a b =
            match b with
            | Some _ -> b
            | None -> a
          ;;
        end)

    include
      Make_to_dyn
        (M)
        (struct
          let field f = Dyn.option f
        end)
  end

  include
    Make_superpose (M) (Partial) (M)
      (struct
        let merge_field a b = Option.value b ~default:a
      end)

  include
    Make_to_dyn
      (M)
      (struct
        let field f = f
      end)

  let standard_watch_exclusions =
    [ {|^_opam|}
    ; {|/_opam|}
    ; {|^_esy|}
    ; {|/_esy|}
    ; {|^\.#.*|} (* Such files can be created by Emacs and also Dune itself. *)
    ; {|/\.#.*|}
    ; {|~$|}
    ; {|^#[^#]*#$|}
    ; {|/#[^#]*#$|}
    ; {|^4913$|} (* https://github.com/neovim/neovim/issues/3460 *)
    ; {|/4913$|}
    ; {|/.git|}
    ; {|/.hg|}
    ; {|:/windows|}
    ]
  ;;

  let hash = Poly.hash
  let equal a b = Poly.equal a b

  let default =
    { display = Simple { verbosity = Quiet; status_line = not Execution_env.inside_dune }
    ; concurrency = (if Execution_env.inside_dune then Fixed 1 else Auto)
    ; terminal_persistence = Clear_on_rebuild
    ; sandboxing_preference = []
    ; cache_enabled = Enabled_except_user_rules
    ; cache_reproducibility_check = Skip
    ; cache_storage_mode = Some (Dune_cache_storage.Mode.default ())
    ; action_stdout_on_success = Print
    ; action_stderr_on_success = Print
    ; project_defaults =
        { authors = Some [ "Author Name <author@example.com>" ]
        ; maintainers = Some [ "Maintainer Name <maintainer@example.com>" ]
        ; maintenance_intent = None
        ; license = Some [ "LICENSE" ]
        }
    ; experimental = []
    }
  ;;

  let decode_generic ~min_dune_version =
    let check min_ver =
      let module S = Dune_lang.Syntax in
      let ver = S.Version.max min_ver min_dune_version in
      let* loc, what = S.desc () in
      S.since_fmt ~fmt:(S.Error_msg.since_config ~what) Stanza.syntax ver loc
    in
    let field_o n v d = field_o n (check v >>> d) in
    let+ display = field_o "display" (1, 0) (enum Display.all)
    and+ concurrency = field_o "jobs" (1, 0) Concurrency.decode
    and+ terminal_persistence =
      field_o "terminal-persistence" (1, 0) Terminal_persistence.decode
    and+ sandboxing_preference =
      field_o "sandboxing_preference" (1, 0) Sandboxing_preference.decode
    and+ cache_enabled = field_o "cache" (2, 0) (Cache.Toggle.decode ~check)
    and+ _cache_transport_unused_since_3_0 =
      field_o
        "cache-transport"
        (2, 0)
        (Dune_lang.Syntax.deleted_in
           Stanza.syntax
           (3, 0)
           ~extra_info:"Dune cache now uses only the direct transport mode."
         >>> Cache.Transport_deprecated.decode)
    and+ cache_check_probability =
      field_o
        "cache-check-probability"
        (2, 7)
        (let+ loc = loc
         and+ p = Dune_lang.Decoder.float in
         loc, p)
    and+ cache_duplication =
      field_o
        "cache-duplication"
        (2, 1)
        (Dune_lang.Syntax.renamed_in Stanza.syntax (3, 0) ~to_:"cache-storage-mode"
         >>> Cache.Storage_mode.decode)
    and+ cache_storage_mode =
      field_o "cache-storage-mode" (3, 0) Cache.Storage_mode.decode
    and+ _cache_trim_period_unused_since_3_0 =
      field_o
        "cache-trim-period"
        (2, 0)
        (Dune_lang.Syntax.deleted_in
           Stanza.syntax
           (3, 0)
           ~extra_info:"To trim the cache, use the 'dune cache trim' command."
         >>> Dune_lang.Decoder.duration)
    and+ _cache_trim_size_unused_since_3_0 =
      field_o
        "cache-trim-size"
        (2, 0)
        (Dune_lang.Syntax.deleted_in
           Stanza.syntax
           (3, 0)
           ~extra_info:"To trim the cache, use the 'dune cache trim' command."
         >>> Dune_lang.Decoder.bytes_unit)
    and+ action_stdout_on_success =
      field_o "action_stdout_on_success" (3, 0) Action_output_on_success.decode
    and+ action_stderr_on_success =
      field_o "action_stderr_on_success" (3, 0) Action_output_on_success.decode
    and+ project_defaults = field_o "project_defaults" (3, 17) Project_defaults.decode
    and+ experimental =
      field_o "experimental" (3, 8) (repeat (pair string (located string)))
    in
    let cache_storage_mode =
      Option.merge cache_duplication cache_storage_mode ~f:(fun _ _ ->
        Code_error.raise "Both cache_duplication and cache_storage_mode are set" [])
    in
    let cache_reproducibility_check =
      Option.map cache_check_probability ~f:(fun (loc, p) ->
        Dune_cache.Config.Reproducibility_check.check_with_probability ~loc p)
    in
    { Partial.display
    ; concurrency
    ; terminal_persistence
    ; sandboxing_preference
    ; cache_enabled
    ; cache_reproducibility_check
    ; cache_storage_mode
    ; action_stdout_on_success
    ; action_stderr_on_success
    ; project_defaults
    ; experimental
    }
  ;;

  let decode =
    fields
      (let+ partial = decode_generic ~min_dune_version:(1, 0)
       and+ () = Dune_lang.Versioned_file.no_more_lang in
       partial)
  ;;

  let decode_fields_of_workspace_file = decode_generic ~min_dune_version:(3, 0)

  let user_config_file =
    let config_dir = Xdg.config_dir (Lazy.force Dune_util.xdg) in
    Path.relative (Path.of_filename_relative_to_initial_cwd config_dir) "dune/config"
  ;;

  include Dune_lang.Versioned_file.Make (struct
      type t = unit
    end)

  let () = Lang.register syntax ()

  let load_config_file p =
    load_exn p ~f:(fun lang ->
      String_with_vars.set_decoding_env (Pform.Env.initial lang.version) decode)
  ;;

  let load_user_config_file () =
    if Path.exists user_config_file
    then load_config_file user_config_file
    else Partial.empty
  ;;

  let adapt_display config ~output_is_a_tty =
    (* Progress isn't meaningful if inside a terminal (or emacs), so disable it if
       the output is getting piped to a file or something. *)
    let config =
      match config.display with
      | Simple { verbosity; _ }
        when (not output_is_a_tty) && not Execution_env.inside_emacs ->
        { config with display = Simple { verbosity; status_line = false } }
      | _ -> config
    in
    (* Similarly, terminal clearing is meaningless if stderr doesn't support ANSI
       codes, so revert-back to Preserve in that case *)
    if config.terminal_persistence = Clear_on_rebuild && not output_is_a_tty
    then { config with terminal_persistence = Terminal_persistence.Preserve }
    else config
  ;;

  let init t ~watch =
    Config.init (String.Map.of_list_exn t.experimental);
    Console.Backend.set (Display.console_backend t.display);
    if watch
    then (
      match t.terminal_persistence with
      | Preserve -> ()
      | Clear_on_rebuild -> Console.reset ()
      | Clear_on_rebuild_and_flush_history -> Console.reset_flush_history ());
    Stdune.Io.set_copy_impl Config.(get copy_file);
    Log.verbose
    := match t.display with
       | Simple { verbosity = Verbose; _ } -> true
       | _ -> false
  ;;

  let auto_concurrency =
    lazy
      (if Sys.win32
       then (
         match Env.get Env.initial "NUMBER_OF_PROCESSORS" with
         | None -> 1
         | Some s -> Int.of_string s |> Option.value ~default:1)
       else (
         let commands =
           [ "nproc", []
           ; "getconf", [ "_NPROCESSORS_ONLN" ]
           ; "getconf", [ "NPROCESSORS_ONLN" ]
           ]
         in
         let rec loop = function
           | [] -> 1
           | (prog, args) :: rest ->
             (match Bin.which ~path:(Env_path.path Env.initial) prog with
              | None -> loop rest
              | Some prog ->
                let prog = Path.to_string prog in
                let fdr, fdw = Unix.pipe () ~cloexec:true in
                (match
                   Spawn.spawn
                     ~prog
                     ~argv:(prog :: args)
                     ~stdin:(Lazy.force Dev_null.in_)
                     ~stdout:fdw
                     ~stderr:(Lazy.force Dev_null.out)
                     ()
                 with
                 | exception Unix.Unix_error _ ->
                   Unix.close fdw;
                   Unix.close fdr;
                   loop commands
                 | pid ->
                   Unix.close fdw;
                   let ic = Unix.in_channel_of_descr fdr in
                   let n =
                     match input_line ic with
                     | line -> String.trim line |> Int.of_string
                     | exception End_of_file -> None
                   in
                   close_in ic;
                   (match n, snd (Unix.waitpid [] pid) with
                    | Some n, WEXITED 0 -> n
                    | _ -> loop rest)))
         in
         loop commands))
  ;;

  let for_scheduler (t : t) ~watch_exclusions stats ~print_ctrl_c_warning =
    let concurrency =
      match t.concurrency with
      | Fixed i -> i
      | Auto ->
        let n = Lazy.force auto_concurrency in
        Log.info [ Pp.textf "Auto-detected concurrency: %d" n ];
        n
    in
    (Dune_engine.Clflags.display
     := match t.display with
        | Tui -> Dune_engine.Display.Quiet
        | Simple { verbosity; _ } -> verbosity);
    { Scheduler.Config.concurrency; stats; print_ctrl_c_warning; watch_exclusions }
  ;;
end
