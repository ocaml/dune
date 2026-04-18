open Stdune
module Category = Category
module Event = Event
module Raw_out = Out

module Out = struct
  include Raw_out

  let cats () =
    let of_string_exn x =
      match Category.of_string x with
      | Some x -> x
      | None -> User_error.raise [ Pp.textf "unrecognized trace category %S" x ]
    in
    match Sys.getenv_opt "DUNE_TRACE" with
    | None -> Category.default
    | Some s ->
      let tokens =
        let dune_trace_re = Re.compile (Re.set ",+-") in
        Re.split_full dune_trace_re s
        |> List.map ~f:(function
          | `Text s -> `Category (of_string_exn s)
          | `Delim g ->
            (match Re.Group.get g 0 with
             | "," -> `Comma
             | "+" -> `Add
             | "-" -> `Remove
             | _ -> assert false))
      in
      if
        List.for_all tokens ~f:(function
          | `Category _ | `Comma -> true
          | _ -> false)
      then
        (* We can do better validation here *)
        List.filter_map tokens ~f:(function
          | `Category x -> Some x
          | _ -> None)
      else (
        let rec loop acc = function
          | `Add :: `Category cat :: xs ->
            let acc = cat :: acc in
            loop acc xs
          | `Remove :: `Category cat :: xs ->
            let acc = List.filter acc ~f:(fun x -> x <> cat) in
            loop acc xs
          | [] -> acc
          | _ :: _ ->
            User_error.raise
              [ Pp.text
                  "invalid DUNE_TRACE. Either specify categories by only ',' or a mix of \
                   '+', and '-' "
              ]
        in
        loop Category.default tokens)
  ;;

  let create path = Raw_out.create (cats ()) path
  let of_fd fd = Raw_out.of_fd (cats ()) fd
end

type ownership =
  | Owned
  | Borrowed

type global =
  { out : Out.t
  ; ownership : ownership
  }

let global = ref None

let at_exit =
  At_exit.at_exit Global_lock.at_exit (fun () ->
    match !global with
    | None -> ()
    | Some { out; ownership = Borrowed } -> Out.close out
    | Some { out; ownership = Owned } ->
      Out.emit out (Event.exit ());
      Out.close out;
      (match Env.(get initial Dune_action_trace.Private.trace_dir_env_var), out.path with
       | None, _ | _, None -> ()
       | Some dir, Some path ->
         let dir = Path.of_string dir in
         Path.mkdir_p dir;
         let dst =
           Path.relative
             (Temp.temp_dir ~parent_dir:dir ~prefix:"dune" ~suffix:"trace")
             "trace.csexp"
         in
         Io.copy_file ~src:path ~dst ()))
;;

let set_global_impl ~ownership out =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some { out; ownership }
;;

let set_global t = set_global_impl ~ownership:Owned t

let set_global_inherited_fd fd =
  let fd = Fd.unsafe_of_unix_file_descr fd in
  Fd.set_close_on_exec fd;
  let out = Out.of_fd (Fd.unsafe_to_unix_file_descr fd) in
  set_global_impl ~ownership:Borrowed out
;;

let set_common_args args = Event.Event.set_common_args args
let global () = Option.map !global ~f:(fun { out; _ } -> out)

let duplicate_global_fd () =
  match global () with
  | None -> None
  | Some out ->
    let fd =
      Unix.dup (Fd.unsafe_to_unix_file_descr out.fd) |> Fd.unsafe_of_unix_file_descr
    in
    Fd.clear_close_on_exec fd;
    Some (Fd.unsafe_to_unix_file_descr fd)
;;

let always_emit event =
  match global () with
  | None -> ()
  | Some out -> Out.emit out event
;;

let emit ?buffered cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem out.cats cat then Out.emit ?buffered out (f ())
;;

let flush () =
  match global () with
  | None -> ()
  | Some out -> Out.flush out
;;

let emit_all ?buffered cat f =
  match global () with
  | None -> ()
  | Some out ->
    if Category.Set.mem out.cats cat then List.iter (f ()) ~f:(Out.emit ?buffered out)
;;

let enabled cat =
  match global () with
  | None -> false
  | Some s -> Category.Set.mem s.cats cat
;;

module Private = struct
  module Fd_count = Fd_count
  module Buffer = Buffer
end
