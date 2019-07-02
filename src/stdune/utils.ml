let signal_name =
  let table =
    let open Sys in
    [ sigabrt   , "ABRT"
    ; sigalrm   , "ALRM"
    ; sigfpe    , "FPE"
    ; sighup    , "HUP"
    ; sigill    , "ILL"
    ; sigint    , "INT"
    ; sigkill   , "KILL"
    ; sigpipe   , "PIPE"
    ; sigquit   , "QUIT"
    ; sigsegv   , "SEGV"
    ; sigterm   , "TERM"
    ; sigusr1   , "USR1"
    ; sigusr2   , "USR2"
    ; sigchld   , "CHLD"
    ; sigcont   , "CONT"
    ; sigstop   , "STOP"
    ; sigtstp   , "TSTP"
    ; sigttin   , "TTIN"
    ; sigttou   , "TTOU"
    ; sigvtalrm , "VTALRM"
    ; sigprof   , "PROF"
    (* These ones are only available in OCaml >= 4.03 *)
    ; -22       , "BUS"
    ; -23       , "POLL"
    ; -24       , "SYS"
    ; -25       , "TRAP"
    ; -26       , "URG"
    ; -27       , "XCPU"
    ; -28       , "XFSZ"
    ]
  in
  fun n ->
    match List.assoc table n with
    | None -> Printf.sprintf "%d\n" n
    | Some s -> s
