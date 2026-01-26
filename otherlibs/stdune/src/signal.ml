type t =
  | Int
  | Term
  | Abrt
  | Alrm
  | Fpe
  | Hup
  | Ill
  | Kill
  | Pipe
  | Quit
  | Segv
  | Usr1
  | Usr2
  | Chld
  | Cont
  | Stop
  | Tstp
  | Ttin
  | Ttou
  | Vtalrm
  | Prof
  | Bus
  | Poll
  | Sys
  | Trap
  | Urg
  | Xcpu
  | Xfsz
  | Winch
  | Unknown of int

external sigwinch : unit -> int = "stdune_winch_number" [@@noalloc]

let all =
  [ Abrt, Sys.sigabrt
  ; Alrm, Sys.sigalrm
  ; Fpe, Sys.sigfpe
  ; Hup, Sys.sighup
  ; Ill, Sys.sigill
  ; Int, Sys.sigint
  ; Kill, Sys.sigkill
  ; Pipe, Sys.sigpipe
  ; Quit, Sys.sigquit
  ; Segv, Sys.sigsegv
  ; Term, Sys.sigterm
  ; Usr1, Sys.sigusr1
  ; Usr2, Sys.sigusr2
  ; Chld, Sys.sigchld
  ; Cont, Sys.sigcont
  ; Stop, Sys.sigstop
  ; Tstp, Sys.sigtstp
  ; Ttin, Sys.sigttin
  ; Ttou, Sys.sigttou
  ; Vtalrm, Sys.sigvtalrm
  ; Prof, Sys.sigprof
  ; Bus, Sys.sigbus
  ; Poll, Sys.sigpoll
  ; Sys, Sys.sigsys
  ; Trap, Sys.sigtrap
  ; Urg, Sys.sigurg
  ; Xcpu, Sys.sigxcpu
  ; Xfsz, Sys.sigxfsz
  ; Winch, sigwinch ()
  ]
;;

let name = function
  | Abrt -> "ABRT"
  | Alrm -> "ALRM"
  | Fpe -> "FPE"
  | Hup -> "HUP"
  | Ill -> "ILL"
  | Int -> "INT"
  | Kill -> "KILL"
  | Pipe -> "PIPE"
  | Quit -> "QUIT"
  | Segv -> "SEGV"
  | Term -> "TERM"
  | Usr1 -> "USR1"
  | Usr2 -> "USR2"
  | Chld -> "CHLD"
  | Cont -> "CONT"
  | Stop -> "STOP"
  | Tstp -> "TSTP"
  | Ttin -> "TTIN"
  | Ttou -> "TTOU"
  | Vtalrm -> "VTALRM"
  | Prof -> "PROF"
  | Bus -> "BUS"
  | Poll -> "POLL"
  | Sys -> "SYS"
  | Trap -> "TRAP"
  | Urg -> "URG"
  | Xcpu -> "XCPU"
  | Xfsz -> "XFSZ"
  | Winch -> "WINCH"
  | Unknown n -> Int.to_string n
;;

let compare (x : t) (y : t) = Poly.compare x y
let equal (x : t) (y : t) = Poly.equal x y

let to_dyn =
  let open Dyn in
  function
  | Unknown n -> variant "Unknown" [ int n ]
  | t -> variant (name t) []
;;

include Comparable.Make (struct
    type nonrec t = t

    let compare = compare
    let to_dyn = to_dyn
  end)

let to_int =
  let table = Map.of_list_exn all in
  function
  | Unknown n -> n
  | t -> Map.find_exn table t
;;

let of_int i =
  let table = Int.Map.of_list_exn (List.map all ~f:(fun (t, i) -> i, t)) in
  match Int.Map.find table i with
  | None -> Unknown i
  | Some s -> s
;;
