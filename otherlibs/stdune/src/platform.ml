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

  let repr =
    Repr.variant
      "platform"
      [ Repr.case0 "Darwin" ~test:(function
          | Darwin -> true
          | _ -> false)
      ; Repr.case0 "Linux" ~test:(function
          | Linux -> true
          | _ -> false)
      ; Repr.case0 "Windows" ~test:(function
          | Windows -> true
          | _ -> false)
      ; Repr.case0 "FreeBSD" ~test:(function
          | FreeBSD -> true
          | _ -> false)
      ; Repr.case0 "NetBSD" ~test:(function
          | NetBSD -> true
          | _ -> false)
      ; Repr.case0 "OpenBSD" ~test:(function
          | OpenBSD -> true
          | _ -> false)
      ; Repr.case0 "Haiku" ~test:(function
          | Haiku -> true
          | _ -> false)
      ; Repr.case0 "Other" ~test:(function
          | Other -> true
          | _ -> false)
      ]
  ;;

  let equal, _ = Repr.make_compare repr

  external is_darwin : unit -> bool = "stdune_is_darwin"
  external is_freebsd : unit -> bool = "stdune_is_freebsd"
  external is_netbsd : unit -> bool = "stdune_is_netbsd"
  external is_openbsd : unit -> bool = "stdune_is_openbsd"
  external is_haiku : unit -> bool = "stdune_is_haiku"

  let to_dyn = Repr.to_dyn repr

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
