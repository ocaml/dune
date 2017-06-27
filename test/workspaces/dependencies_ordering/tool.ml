let () =
  match !Config.mode with
  | Config.Cmdline -> Printf.printf "I'm a cmdline tool I must print log in stdout"
  | Config.Gui -> Printf.printf "I'm a gui tool I must print log in the interface"
