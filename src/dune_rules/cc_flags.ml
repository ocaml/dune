open Import
open Memo.O

type cc_vendor =
  | Gcc
  | Clang
  | Msvc
  | Mingw
  | Intel
  | Xlc
  | Sun
  | Other of string

type phase =
  | Compile of Ocaml.Version.t
  | Link

let base_cxx_compile_flags version = function
  | Gcc | Clang | Mingw | Intel ->
    "-x"
    :: "c++"
    :: (if Ocaml.Version.add_std_cxx_flag version then [ "-std=gnu++11" ] else [])
  | Msvc -> [ "/TP" ]
  | Xlc | Sun | Other _ -> []
;;

let base_cxx_link_flags = function
  | Gcc | Mingw | Intel -> [ "-lstdc++"; "-shared-libgcc" ]
  | Clang -> [ "-lc++" ]
  | Msvc | Xlc | Sun | Other _ -> []
;;

let fdiagnostics_color = function
  | Gcc | Clang | Mingw | Intel -> [ "-fdiagnostics-color=always" ]
  | _ -> []
;;

let warnings = function
  | Gcc | Clang | Mingw | Intel -> [ "-Wall" ]
  | Msvc -> [ "-W2" ]
  | _ -> []
;;

let header_file_content =
  {|#if defined(_MSC_VER)
msvc
#elif defined(__INTEL_COMPILER)
icc
#elif defined(__MINGW32__)
mingw
#elif defined(__clang_major__) && defined(__clang_minor__)
clang
#elif defined(__GNUC__) && defined(__GNUC_MINOR__)
gcc
#elif defined(__xlc__) && defined(__xlC__)
xlc
#elif defined(__SUNPRO_C)
sunc
#else
unknown
#endif
|}
;;

module Detect = struct
  module Spec = struct
    type ('path, _) t =
      { c_compiler : 'path
      ; ccomp_type : Ocaml_config.Ccomp_type.t
      }

    let name = "detect-cc-vendor"
    let version = 1
    let bimap t f _ = { t with c_compiler = f t.c_compiler }
    let is_useful_to ~memoize:_ = true
    let runs_process = true
    let can_run_in_action_runner = true

    let encode { c_compiler; ccomp_type } path _target =
      Sexp.List
        [ path c_compiler
        ; Atom (Ocaml_config.Ccomp_type.to_string ccomp_type)
        ; Atom header_file_content
        ]
    ;;

    let args ccomp_type file =
      let file = Path.to_string file in
      match ccomp_type with
      | Ocaml_config.Ccomp_type.Msvc -> [ "/nologo"; "/EP"; file ]
      | Cc | Other _ -> [ "-E"; "-P"; file ]
    ;;

    let action { c_compiler; ccomp_type } ~(ectx : Action.context) ~(eenv : Action.env) =
      let header = Temp.create File ~prefix:"dune-cc-vendor" ~suffix:".h" in
      Fiber.finalize
        (fun () ->
           Io.write_file header header_file_content;
           Process.run
             Strict
             c_compiler
             (args ccomp_type header)
             ~display:!Clflags.display
             ~metadata:ectx.metadata
             ~stdout_to:eenv.stdout_to
             ~stderr_to:eenv.stderr_to
             ~stdin_from:eenv.stdin_from
             ~dir:eenv.working_dir
             ~env:eenv.env)
        ~finally:(fun () ->
          Temp.destroy File header;
          Fiber.return ())
    ;;
  end

  include Action_ext.Make (Spec)
end

let parse_cc_vendor cc_vendor =
  match String.trim cc_vendor with
  | "msvc" -> Msvc
  | "icc" -> Intel
  | "mingw" -> Mingw
  | "clang" -> Clang
  | "gcc" -> Gcc
  | "xlc" -> Xlc
  | "sunc" -> Sun
  | s -> Other s
;;

let resolve_c_compiler context ~dir program =
  match Filename.analyze_program_name program with
  | Absolute -> Memo.return (Ok (Path.of_filename_relative_to_initial_cwd program))
  | Relative_to_current_dir -> Memo.return (Ok (Path.relative (Path.build dir) program))
  | In_path ->
    let program = Filename.of_string_exn program in
    Context.which ~narrow_to_packages:None context program
    >>| (function
     | Some path -> Ok path
     | None ->
       Error
         (Action.Prog.Not_found.create
            ~context:(Context.name context)
            ~program
            ~loc:None
            ()))
;;

let cc_vendor_action (ctx : Build_context.t) =
  let open Action_builder.O in
  let* context = Action_builder.of_memo (Context.DB.get ctx.name) in
  let* ocaml_config =
    Action_builder.of_memo
      (let open Memo.O in
       let+ ocaml = Context.ocaml context in
       ocaml.ocaml_config)
  in
  Ocaml_config.c_compiler ocaml_config
  |> resolve_c_compiler context ~dir:ctx.build_dir
  |> Action_builder.of_memo
  >>= function
  | Error e -> Action_builder.fail { fail = (fun () -> Action.Prog.Not_found.raise e) }
  | Ok c_compiler ->
    let+ action =
      let+ () = Action_builder.path c_compiler
      and+ env = Action_builder.of_memo (Context.installed_env context) in
      Detect.action { c_compiler; ccomp_type = Ocaml_config.ccomp_type ocaml_config }
      |> Action.chdir (Path.build ctx.build_dir)
      |> Action.Full.make
      |> Action.Full.add_env env
    in
    { Rule.Anonymous_action.action; loc = Loc.none; dir = ctx.build_dir; alias = None }
;;

let check_warn = function
  | Other s ->
    User_warning.emit
      [ Pp.textf
          "Dune was not able to automatically infer the C/C++ compiler in use: \"%s\". \
           Please open an issue on GitHub to help us improve this feature."
          s
      ]
  | _ -> ()
;;

let cc_vendor (ctx : Build_context.t) =
  let open Action_builder.O in
  let+ cc_vendor =
    cc_vendor_action ctx
    |> Build_system.execute_action_stdout
    |> Memo.map ~f:parse_cc_vendor
    |> Action_builder.of_memo
  in
  check_warn cc_vendor;
  cc_vendor
;;

let get_flags ~for_ ctx =
  let open Action_builder.O in
  let+ cc_vendor = cc_vendor ctx in
  (match for_ with
   | Compile version -> base_cxx_compile_flags version
   | Link -> base_cxx_link_flags)
    cc_vendor
;;
