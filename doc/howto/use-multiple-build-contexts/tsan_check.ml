let x = ref 0
let n = 5_000_000

let inc () =
  for _ = 1 to n do
    x := !x + 1
  done
;;

let () =
  let d = Domain.spawn inc in
  inc ();
  Domain.join d;
  let expected = 2 * n in
  Printf.printf "x=%d expected=%d\n%!" !x expected;
  assert (!x = expected)
;;
