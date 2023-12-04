module OS = struct
  (* CR-someday alizter: Include mingw32, mongw64, cygwin *)
  type t =
    | Darwin
    | Linux
    | Windows
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Haiku
    | Other

  let equal = Poly.equal

  external is_darwin : unit -> bool = "stdune_is_darwin"
  external is_freebsd : unit -> bool = "stdune_is_freebsd"
  external is_netbsd : unit -> bool = "stdune_is_netbsd"
  external is_openbsd : unit -> bool = "stdune_is_openbsd"
  external is_haiku : unit -> bool = "stdune_is_haiku"

  let to_dyn : t -> Dyn.t = function
    | Windows -> Dyn.variant "Windows" []
    | Darwin -> Dyn.variant "Darwin" []
    | Linux -> Dyn.variant "Linux" []
    | FreeBSD -> Dyn.variant "FreeBSD" []
    | NetBSD -> Dyn.variant "NetBSD" []
    | OpenBSD -> Dyn.variant "OpenBSD" []
    | Haiku -> Dyn.variant "Haiku" []
    | Other -> Dyn.variant "Other" []
  ;;

  let is_linux () =
    try
      let chan = open_in_bin "/proc/sys/kernel/ostype" in
      Exn.protect
        ~f:(fun () ->
          match String.trim (input_line chan) with
          | "Linux" -> true
          | _ -> false)
        ~finally:(fun () -> close_in_noerr chan)
    with
    | _ -> false
  ;;

  let value =
    if Stdlib.Sys.win32
    then Windows
    else if is_darwin ()
    then Darwin
    else if is_linux ()
    then Linux
    else if is_freebsd ()
    then FreeBSD
    else if is_netbsd ()
    then NetBSD
    else if is_openbsd ()
    then OpenBSD
    else if is_haiku ()
    then Haiku
    else Other
  ;;
end

let assert_os what =
  if not (OS.equal OS.value what)
  then Code_error.raise "unexpected os" [ ("os", OS.(to_dyn value)) ]
;;
