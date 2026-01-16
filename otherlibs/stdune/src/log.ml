let sprintf = Printf.sprintf

module Message = struct
  type level =
    [ `Warn
    | `Info
    | `Verbose
    ]

  let string_of_level = function
    | `Warn -> "Warn"
    | `Info -> "Info"
    | `Verbose -> "Verbose"
  ;;

  type t =
    { level : level
    ; message : string
    ; args : (string * Dyn.t) list
    }

  let ocamlparam () =
    { level = `Info
    ; message = "ocamlparam"
    ; args =
        [ ( "OCAMLPARAM"
          , Dyn.string
              (match Env.get Env.initial "OCAMLPARAM" with
               | Some s -> Printf.sprintf "%S" s
               | None -> "unset") )
        ]
    }
  ;;

  let create level message args = { level; message; args }
end

module File = struct
  type t =
    | Redirect of (Message.t -> unit)
    | No_log_file
    | Stderr
    | Both of t * t
end

type real =
  | No_log_file
  | Redirect of (Message.t -> unit)
  | Output of out_channel
  | Both of real * real

let t = Fdecl.create Dyn.opaque
let verbose = ref false

let format_args args =
  List.map args ~f:(fun (k, v) -> Printf.sprintf "%s: %s" k (Dyn.to_string v))
  |> String.concat ~sep:" "
;;

let write_oc oc { Message.level; message; args } =
  output_string
    oc
    (sprintf "%s %s | %s\n" (Message.string_of_level level) message (format_args args))
;;

let rec emit (t : real) (message : Message.t) =
  match t with
  | No_log_file -> ()
  | Redirect w -> w message
  | Output oc -> write_oc oc message
  | Both (x, y) ->
    emit x message;
    emit y message
;;

let init (dst : File.t) =
  let rec make (dst : File.t) =
    match dst with
    | No_log_file -> No_log_file
    | Stderr -> Output stderr
    | Redirect w -> Redirect w
    | Both (x, y) -> Both (make x, make y)
  in
  Fdecl.set t (make dst);
  emit (Fdecl.get t) (Message.ocamlparam ())
;;

let t () = Fdecl.get t
let forward_verbose = Fdecl.create Dyn.opaque
let set_forward_verbose = Fdecl.set forward_verbose
let log f = emit (t ()) (f ())

let info message args =
  emit (t ()) { Message.level = `Info; message; args };
  if !verbose then (Fdecl.get forward_verbose) message args
;;

let warn message args = info ("Warning: " ^ message) args

let command ~command_line ~output ~exit_status =
  if !verbose
  then (
    let command_line = Ansi_color.strip command_line in
    let exit_status =
      match (exit_status : Unix.process_status) with
      | WEXITED n -> Printf.sprintf "[%d]" n
      | WSIGNALED n ->
        let name = Signal.of_int n |> Signal.name in
        Printf.sprintf "[got signal %s]" name
      | WSTOPPED _ -> assert false
    in
    emit
      (t ())
      { Message.level = `Info
      ; message = "command"
      ; args =
          [ "command_line", Dyn.string command_line
          ; "outupt", Dyn.string output
          ; "exit_status", Dyn.string exit_status
          ]
      })
;;

let verbose_message m args = if !verbose then info m args
