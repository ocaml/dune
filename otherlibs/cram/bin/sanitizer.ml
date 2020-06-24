open Stdune

module Command = struct
  type t =
    { output : string
    ; build_path_prefix_map : string
    }

  let of_sexp (csexp : Sexp.t) : t =
    match csexp with
    | List [ Atom build_path_prefix_map; Atom output ] ->
      { build_path_prefix_map; output }
    | _ -> Code_error.raise "Command.of_csexp: invalid csexp" []

  let to_sexp { output; build_path_prefix_map } : Sexp.t =
    List [ Atom build_path_prefix_map; Atom output ]
end

let run_sanitizer ?temp_dir ~prog ~argv commands =
  let temp_dir =
    match temp_dir with
    | Some d -> d
    | None -> Temp.create Dir ~prefix:"sanitizer" ~suffix:"unspecified"
  in
  let argv = prog :: argv in
  let fname = Path.relative temp_dir in
  let stdout_path = fname "sanitizer.stdout" in
  let stdout =
    Unix.openfile
      (Path.to_string stdout_path)
      [ Unix.O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ]
      0o777
  in
  let stdin =
    let path = fname "sanitizer.stdin" in
    let csexp = List.map commands ~f:Command.to_sexp in
    Io.with_file_out ~binary:true path ~f:(fun oc ->
        List.iter csexp ~f:(Csexp.to_channel oc));
    Unix.openfile (Path.to_string path) [ Unix.O_RDONLY; O_SHARE_DELETE ] 0o666
  in
  let pid = Spawn.spawn ~prog ~argv ~stdin ~stdout () in
  Unix.close stdout;
  Unix.close stdin;
  match Unix.waitpid [] (Pid.to_int pid) with
  | _, WEXITED 0 ->
    Io.with_file_in stdout_path ~f:(fun ic ->
        let rec loop acc =
          match Csexp.input_opt ic with
          | Ok None -> List.rev acc
          | Ok (Some (Sexp.Atom s)) -> loop (s :: acc)
          | Error error ->
            Code_error.raise "invalid csexp" [ ("error", String error) ]
          | Ok _ -> Code_error.raise "unexpected output" []
        in
        loop [])
  | _ -> Code_error.raise "unexpected termination of sanitizer" []

let impl_sanitizer f in_ out =
  let rec loop () =
    match Csexp.input_opt in_ with
    | Error error ->
      Code_error.raise "unable to parse csexp" [ ("error", String error) ]
    | Ok None -> ()
    | Ok (Some sexp) ->
      let command = Command.of_sexp sexp in
      Csexp.to_channel out (Atom (f command));
      flush out;
      loop ()
  in
  loop ()
