let bytes_conversion_table = [ [ "B"; "bytes" ], 1L ]

let rec long_power (l : int64) (n : int) : int64 =
  if n = 0 then 1L else Int64.mul l @@ long_power l (n - 1)
;;

let decimal_conversion_table =
  [ [ "kB"; "KB"; "kilobytes" ], 1_000L
  ; [ "MB"; "megabytes" ], long_power 1_000L 2
  ; [ "GB"; "gigabytes" ], long_power 1_000L 3
  ; [ "TB"; "terabytes" ], long_power 1_000L 4
  ]
;;

let binary_conversion_table =
  [ [ "KiB"; "KiB"; "kibibytes" ], 1024L
  ; [ "MiB"; "mebibytes" ], long_power 1024L 2
  ; [ "GiB"; "gibibytes" ], long_power 1024L 3
  ; [ "TiB"; "tebibytes" ], long_power 1024L 4
  ]
;;

(* When printing we only use this conversion table *)
let conversion_table = bytes_conversion_table @ decimal_conversion_table

let pp x =
  (* We go through the list to find the first unit that is greater than the
     number of bytes and take the predecessor as the units for printing. For the
     special base case where no conversion is necessary we don't print as a
     float. *)
  let suffix, value =
    let rec loop = function
      | [] -> assert false
      | [ (units, value) ] -> List.hd units, value
      | (units, value) :: ((_, value') :: _ as l) ->
        if x = 0L
        then List.hd units, value
        else if value <= x && x < value'
        then List.hd units, value
        else loop l
    in
    loop @@ conversion_table
  in
  if value = 1L
  then Printf.sprintf "%Ld%s" x suffix
  else Printf.sprintf "%.2f%s" (Int64.to_float x /. Int64.to_float value) suffix
;;

(* When parsing we accept all units *)
let conversion_table =
  bytes_conversion_table @ decimal_conversion_table @ binary_conversion_table
  |> List.sort ~compare:(fun (_, x) (_, y) -> Ordering.of_int @@ Int64.compare x y)
;;
