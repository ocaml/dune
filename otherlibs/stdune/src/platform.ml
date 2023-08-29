module OS = struct
  (* CR-someday alizter: Include mingw32, mongw64, cygwin *)
  type t =
    [ `Darwin
    | `Linux
    | `Windows
    | `FreeBSD
    | `NetBSD
    | `OpenBSD
    | `DragonFly
    | `Haiku
    | `Serenity
    | `Solaris
    | `Other
    ]

  (* Include all constructors here. *)
  let all =
    [ `Darwin
    ; `Linux
    ; `Windows
    ; `FreeBSD
    ; `NetBSD
    ; `OpenBSD
    ; `DragonFly
    ; `Haiku
    ; `Serenity
    ; `Solaris
    ; `Other
    ]
  ;;

  type fswatch_support =
    [ `Darwin
    | `Linux
    | `FreeBSD
    | `NetBSD
    | `OpenBSD
    | `DragonFly
    | `Solaris
    ]

  let equal = Poly.equal

  external is_darwin : unit -> bool = "stdune_is_darwin"
  external is_freebsd : unit -> bool = "stdune_is_freebsd"
  external is_netbsd : unit -> bool = "stdune_is_netbsd"
  external is_openbsd : unit -> bool = "stdune_is_openbsd"
  external is_dragonfly : unit -> bool = "stdune_is_dragonfly"
  external is_haiku : unit -> bool = "stdune_is_haiku"
  external is_serenity : unit -> bool = "stdune_is_serenity"
  external is_solaris : unit -> bool = "stdune_is_solaris"

  let to_dyn : t -> Dyn.t = function
    | `Windows -> Dyn.variant "Windows" []
    | `Darwin -> Dyn.variant "Darwin" []
    | `Linux -> Dyn.variant "Linux" []
    | `FreeBSD -> Dyn.variant "FreeBSD" []
    | `NetBSD -> Dyn.variant "NetBSD" []
    | `OpenBSD -> Dyn.variant "OpenBSD" []
    | `DragonFly -> Dyn.variant "DragonFly" []
    | `Haiku -> Dyn.variant "Haiku" []
    | `Serenity -> Dyn.variant "Serenity" []
    | `Solaris -> Dyn.variant "Solaris" []
    | `Other -> Dyn.variant "Other" []
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

  let detect = function
    | `Windows -> Stdlib.Sys.win32
    | `Darwin -> is_darwin ()
    | `Linux -> is_linux ()
    | `FreeBSD -> is_freebsd ()
    | `NetBSD -> is_netbsd ()
    | `OpenBSD -> is_openbsd ()
    | `DragonFly -> is_dragonfly ()
    | `Haiku -> is_haiku ()
    | `Serenity -> is_serenity ()
    | `Solaris -> is_solaris ()
    | `Other -> false
  ;;

  let value =
    match List.find all ~f:detect with
    | Some os -> os
    | None -> `Other
  ;;
end

let assert_os what =
  if not (OS.equal OS.value what)
  then Code_error.raise "unexpected os" [ ("os", OS.(to_dyn value)) ]
;;
