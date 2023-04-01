module OS = struct
  type t =
    | Darwin
    | Linux
    | Windows
    | Other

  let equal = Poly.equal

  external is_darwin : unit -> bool = "stdune_is_darwin"

  let to_dyn = function
    | Windows -> Dyn.variant "Windows" []
    | Darwin -> Dyn.variant "Darwin" []
    | Linux -> Dyn.variant "Linux" []
    | Other -> Dyn.variant "Other" []

  let linux () =
    try
      let chan = open_in_bin "/proc/sys/kernel/ostype" in
      Exn.protect
        ~f:(fun () ->
          match String.trim (input_line chan) with
          | "Linux" -> true
          | _ -> false)
        ~finally:(fun () -> close_in_noerr chan)
    with _ -> false

  let value =
    if Stdlib.Sys.win32 then Windows
    else if is_darwin () then Darwin
    else if linux () then Linux
    else Other
end

let assert_os what =
  if not (OS.equal OS.value what) then
    Code_error.raise "unexpected os" [ ("os", OS.(to_dyn value)) ]
