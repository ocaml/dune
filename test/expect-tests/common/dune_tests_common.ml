open Stdune
module Console = Dune_console

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

let init =
  let init =
    lazy
      (Printexc.record_backtrace false;
       Path.set_root (Path.External.cwd ());
       Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
       Console.Backend.(set dumb);
       Log.init No_log_file)
  in
  fun () -> Lazy.force init
;;
