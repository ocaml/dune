module String = struct
  let is_prefix ~prefix str =
    let pl = String.length prefix in
    if String.length str < pl then
      false
    else
      String.sub str 0 (String.length prefix) = prefix

  let is_suffix ~suffix str =
    let pl = String.length suffix in
    if String.length str < pl then
      false
    else
      String.sub str (String.length str - pl) pl = suffix

  let cut sep str =
    try
      let idx = String.index str sep
      and l = String.length str
      in
      let sidx = succ idx in
      Some (String.sub str 0 idx, String.sub str sidx (l - sidx))
    with
      Not_found -> None

  let cuts sep str =
    let rec doit acc s =
      match cut sep s with
      | None -> List.rev (s :: acc)
      | Some (a, b) -> doit (a :: acc) b
    in
    doit [] str

  let slice ?(start = 0) ?stop str =
    let stop = match stop with
      | None -> String.length str
      | Some x -> x
    in
    let len = stop - start in
    String.sub str start len

  let count_common_suffix x y =
    let rec loop ~x ~y acc ix iy =
      if ix >= 0 && iy >= 0 &&
         String.unsafe_get x ix = (String.unsafe_get y iy : char) then
        loop ~x ~y (acc + 1) (ix - 1) (iy - 1)
      else
        acc
    in
    let len_x = String.length x in
    let len_y = String.length y in
    loop ~x ~y 0 (len_x - 1) (len_y - 1)
end

module List = struct
  let rec last = function
    | [] -> invalid_arg "List.last"
    | [x] -> x
    | _::xs -> last xs
end
