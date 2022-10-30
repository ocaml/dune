open Stdune
module Flock = Dune_util.Flock

let%expect_test "blocking lock" =
  let fd = Unix.openfile "tlc1" [ Unix.O_CREAT ] 0o777 in
  let lock = Flock.create fd in
  print_endline "acquiring lock";
  (match Flock.lock_block lock Exclusive with
  | Ok () -> print_endline "acquired lock"
  | Error _ -> assert false);
  (match Flock.unlock lock with
  | Ok () -> print_endline "released lock"
  | Error _ -> assert false);
  Unix.close fd;
  [%expect {|
    acquiring lock
    acquired lock
    released lock |}]

let%expect_test "nonblocking lock" =
  let fd flag = Unix.openfile "tlc2" [ flag ] 0o777 in
  let fd1 = fd Unix.O_CREAT in
  let lock1 = Flock.create fd1 in
  print_endline "acquiring lock";
  (match Flock.lock_non_block lock1 Exclusive with
  | Ok `Success -> print_endline "acquired lock"
  | Ok `Failure | Error _ -> assert false);
  let fd2 = fd Unix.O_RDONLY in
  let lock2 = Flock.create fd2 in
  (match Flock.lock_non_block lock2 Exclusive with
  | Ok `Failure -> print_endline "verified that we can't lock again"
  | Ok `Success -> Code_error.raise "acquired lock again" []
  | Error err ->
    Code_error.raise "err" [ ("message", Dyn.string @@ Unix.error_message err) ]);
  (match Flock.unlock lock1 with
  | Ok () -> print_endline "released lock"
  | Error _ -> assert false);
  let lock2 = Flock.create fd2 in
  (match Flock.lock_non_block lock2 Exclusive with
  | Ok `Success -> print_endline "managed to lock after unlock"
  | Ok `Failure | Error _ -> assert false);
  Unix.close fd1;
  Unix.close fd2;
  [%expect
    {|
    acquiring lock
    acquired lock
    verified that we can't lock again
    released lock
    managed to lock after unlock |}]

let%expect_test "double lock" =
  let fd = Unix.openfile "tlc3" [ Unix.O_CREAT ] 0o600 in
  let lock = Flock.create fd in
  (match Flock.lock_non_block lock Exclusive with
  | Ok `Success -> print_endline "lock 1 worked"
  | _ -> assert false);
  (match Flock.lock_non_block lock Exclusive with
  | Ok `Success -> print_endline "lock 2 worked"
  | _ -> assert false);
  [%expect {|
    lock 1 worked
    lock 2 worked |}]
