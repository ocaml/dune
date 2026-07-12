open Import

type command =
  { prog : string
  ; argv : string list
  }

let available = Stdune.Landlock.available

let restrict_shared_cache_to_read_only () =
  let build_cache_dir = Lazy.force Dune_cache.Layout.build_cache_dir in
  Path.mkdir_p build_cache_dir;
  let deny_write =
    [ Path.to_absolute_filename build_cache_dir |> Path.External.of_string ]
  in
  Stdune.Landlock.Policy.create ~deny_write ~allow_write:[]
  |> Stdune.Landlock.restrict_self
;;

let wrap ~dune_prog argv =
  if available ()
  then (
    let prog = Path.to_string dune_prog in
    Some { prog; argv = [ prog; "internal"; "with-landlock"; "--" ] @ argv })
  else None
;;

let wrap_exn ~dune_prog argv =
  match wrap ~dune_prog argv with
  | Some command -> command
  | None -> User_error.raise [ Pp.text "Landlock is not available on this system" ]
;;

module With_landlock = struct
  let command =
    let doc = "Run a command under Dune's Landlock wrapper." in
    let info = Cmd.info "with-landlock" ~doc in
    let term =
      let+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
      match argv with
      | [] -> User_error.raise [ Pp.text "missing command after --" ]
      | prog :: args ->
        restrict_shared_cache_to_read_only ();
        Proc.restore_cwd_and_execve (Util.resolve_prog prog) args ~env:Env.initial
    in
    Cmd.v info term
  ;;
end
