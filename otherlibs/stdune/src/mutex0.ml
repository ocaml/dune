open Mutex

let[@warning "-32"] [@inline never] protect mutex f =
  lock mutex;
  match f () with
  | x ->
    unlock mutex;
    x
  | exception exn ->
    unlock mutex;
    raise exn
;;

include Mutex
