(* CR-someday amokhov: Add KiB, MiB, GiB. *)
let conversion_table =
  [ ([ "B"; "bytes" ], 1L)
  ; ([ "kB"; "KB"; "kilobytes" ], 1_000L)
  ; ([ "MB"; "megabytes" ], 1_000_000L)
  ; ([ "GB"; "gigabytes" ], 1_000_000_000L)
  ; ([ "TB"; "terabytes" ], 1_000_000_000_000L)
  ]

let pp x =
  (* We go through the list to find the first unit that is greater than the
     number of bytes and take the predecessor as the units for printing. For the
     special base case where no conversion is necessary we don't print as a
     float. *)
  let suffix, value =
    let rec loop = function
      | [] -> assert false
      | [ (units, value) ] -> (List.hd units, value)
      | (units, value) :: ((_, value') :: _ as l) ->
        if x = 0L then (List.hd units, value)
        else if value <= x && x < value' then (List.hd units, value)
        else loop l
    in
    loop @@ conversion_table
  in
  if value = 1L then Printf.sprintf "%Ld%s" x suffix
  else Printf.sprintf "%.2f%s" (Int64.to_float x /. Int64.to_float value) suffix
