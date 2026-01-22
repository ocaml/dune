type t = Unix.error

(* CR-someday amokhov: It would be nice to derive this instead. For now let's
     trust I haven't messed this up. *)
let equal (x : t) (y : t) =
  match x, y with
  | E2BIG, E2BIG -> true
  | E2BIG, _ | _, E2BIG -> false
  | EACCES, EACCES -> true
  | EACCES, _ | _, EACCES -> false
  | EAGAIN, EAGAIN -> true
  | EAGAIN, _ | _, EAGAIN -> false
  | EBADF, EBADF -> true
  | EBADF, _ | _, EBADF -> false
  | EBUSY, EBUSY -> true
  | EBUSY, _ | _, EBUSY -> false
  | ECHILD, ECHILD -> true
  | ECHILD, _ | _, ECHILD -> false
  | EDEADLK, EDEADLK -> true
  | EDEADLK, _ | _, EDEADLK -> false
  | EDOM, EDOM -> true
  | EDOM, _ | _, EDOM -> false
  | EEXIST, EEXIST -> true
  | EEXIST, _ | _, EEXIST -> false
  | EFAULT, EFAULT -> true
  | EFAULT, _ | _, EFAULT -> false
  | EFBIG, EFBIG -> true
  | EFBIG, _ | _, EFBIG -> false
  | EINTR, EINTR -> true
  | EINTR, _ | _, EINTR -> false
  | EINVAL, EINVAL -> true
  | EINVAL, _ | _, EINVAL -> false
  | EIO, EIO -> true
  | EIO, _ | _, EIO -> false
  | EISDIR, EISDIR -> true
  | EISDIR, _ | _, EISDIR -> false
  | EMFILE, EMFILE -> true
  | EMFILE, _ | _, EMFILE -> false
  | EMLINK, EMLINK -> true
  | EMLINK, _ | _, EMLINK -> false
  | ENAMETOOLONG, ENAMETOOLONG -> true
  | ENAMETOOLONG, _ | _, ENAMETOOLONG -> false
  | ENFILE, ENFILE -> true
  | ENFILE, _ | _, ENFILE -> false
  | ENODEV, ENODEV -> true
  | ENODEV, _ | _, ENODEV -> false
  | ENOENT, ENOENT -> true
  | ENOENT, _ | _, ENOENT -> false
  | ENOEXEC, ENOEXEC -> true
  | ENOEXEC, _ | _, ENOEXEC -> false
  | ENOLCK, ENOLCK -> true
  | ENOLCK, _ | _, ENOLCK -> false
  | ENOMEM, ENOMEM -> true
  | ENOMEM, _ | _, ENOMEM -> false
  | ENOSPC, ENOSPC -> true
  | ENOSPC, _ | _, ENOSPC -> false
  | ENOSYS, ENOSYS -> true
  | ENOSYS, _ | _, ENOSYS -> false
  | ENOTDIR, ENOTDIR -> true
  | ENOTDIR, _ | _, ENOTDIR -> false
  | ENOTEMPTY, ENOTEMPTY -> true
  | ENOTEMPTY, _ | _, ENOTEMPTY -> false
  | ENOTTY, ENOTTY -> true
  | ENOTTY, _ | _, ENOTTY -> false
  | ENXIO, ENXIO -> true
  | ENXIO, _ | _, ENXIO -> false
  | EPERM, EPERM -> true
  | EPERM, _ | _, EPERM -> false
  | EPIPE, EPIPE -> true
  | EPIPE, _ | _, EPIPE -> false
  | ERANGE, ERANGE -> true
  | ERANGE, _ | _, ERANGE -> false
  | EROFS, EROFS -> true
  | EROFS, _ | _, EROFS -> false
  | ESPIPE, ESPIPE -> true
  | ESPIPE, _ | _, ESPIPE -> false
  | ESRCH, ESRCH -> true
  | ESRCH, _ | _, ESRCH -> false
  | EXDEV, EXDEV -> true
  | EXDEV, _ | _, EXDEV -> false
  | EWOULDBLOCK, EWOULDBLOCK -> true
  | EWOULDBLOCK, _ | _, EWOULDBLOCK -> false
  | EINPROGRESS, EINPROGRESS -> true
  | EINPROGRESS, _ | _, EINPROGRESS -> false
  | EALREADY, EALREADY -> true
  | EALREADY, _ | _, EALREADY -> false
  | ENOTSOCK, ENOTSOCK -> true
  | ENOTSOCK, _ | _, ENOTSOCK -> false
  | EDESTADDRREQ, EDESTADDRREQ -> true
  | EDESTADDRREQ, _ | _, EDESTADDRREQ -> false
  | EMSGSIZE, EMSGSIZE -> true
  | EMSGSIZE, _ | _, EMSGSIZE -> false
  | EPROTOTYPE, EPROTOTYPE -> true
  | EPROTOTYPE, _ | _, EPROTOTYPE -> false
  | ENOPROTOOPT, ENOPROTOOPT -> true
  | ENOPROTOOPT, _ | _, ENOPROTOOPT -> false
  | EPROTONOSUPPORT, EPROTONOSUPPORT -> true
  | EPROTONOSUPPORT, _ | _, EPROTONOSUPPORT -> false
  | ESOCKTNOSUPPORT, ESOCKTNOSUPPORT -> true
  | ESOCKTNOSUPPORT, _ | _, ESOCKTNOSUPPORT -> false
  | EOPNOTSUPP, EOPNOTSUPP -> true
  | EOPNOTSUPP, _ | _, EOPNOTSUPP -> false
  | EPFNOSUPPORT, EPFNOSUPPORT -> true
  | EPFNOSUPPORT, _ | _, EPFNOSUPPORT -> false
  | EAFNOSUPPORT, EAFNOSUPPORT -> true
  | EAFNOSUPPORT, _ | _, EAFNOSUPPORT -> false
  | EADDRINUSE, EADDRINUSE -> true
  | EADDRINUSE, _ | _, EADDRINUSE -> false
  | EADDRNOTAVAIL, EADDRNOTAVAIL -> true
  | EADDRNOTAVAIL, _ | _, EADDRNOTAVAIL -> false
  | ENETDOWN, ENETDOWN -> true
  | ENETDOWN, _ | _, ENETDOWN -> false
  | ENETUNREACH, ENETUNREACH -> true
  | ENETUNREACH, _ | _, ENETUNREACH -> false
  | ENETRESET, ENETRESET -> true
  | ENETRESET, _ | _, ENETRESET -> false
  | ECONNABORTED, ECONNABORTED -> true
  | ECONNABORTED, _ | _, ECONNABORTED -> false
  | ECONNRESET, ECONNRESET -> true
  | ECONNRESET, _ | _, ECONNRESET -> false
  | ENOBUFS, ENOBUFS -> true
  | ENOBUFS, _ | _, ENOBUFS -> false
  | EISCONN, EISCONN -> true
  | EISCONN, _ | _, EISCONN -> false
  | ENOTCONN, ENOTCONN -> true
  | ENOTCONN, _ | _, ENOTCONN -> false
  | ESHUTDOWN, ESHUTDOWN -> true
  | ESHUTDOWN, _ | _, ESHUTDOWN -> false
  | ETOOMANYREFS, ETOOMANYREFS -> true
  | ETOOMANYREFS, _ | _, ETOOMANYREFS -> false
  | ETIMEDOUT, ETIMEDOUT -> true
  | ETIMEDOUT, _ | _, ETIMEDOUT -> false
  | ECONNREFUSED, ECONNREFUSED -> true
  | ECONNREFUSED, _ | _, ECONNREFUSED -> false
  | EHOSTDOWN, EHOSTDOWN -> true
  | EHOSTDOWN, _ | _, EHOSTDOWN -> false
  | EHOSTUNREACH, EHOSTUNREACH -> true
  | EHOSTUNREACH, _ | _, EHOSTUNREACH -> false
  | ELOOP, ELOOP -> true
  | ELOOP, _ | _, ELOOP -> false
  | EOVERFLOW, EOVERFLOW -> true
  | EOVERFLOW, _ | _, EOVERFLOW -> false
  | EUNKNOWNERR x, EUNKNOWNERR y -> Int.equal x y
;;

module Detailed = struct
  type nonrec t = t * string * string

  let raise (e, x, y) = raise (Unix.Unix_error (e, x, y))
  let create error ~syscall ~arg = error, syscall, arg

  let catch f x =
    match f x with
    | res -> Ok res
    | exception Unix.Unix_error (error, syscall, arg) ->
      Error (create error ~syscall ~arg)
  ;;

  let equal (a1, b1, c1) (a2, b2, c2) =
    equal a1 a2 && String.equal b1 b2 && String.equal c1 c2
  ;;

  let to_string_hum (error, syscall, arg) =
    Format.sprintf "%s(%s): %s" syscall arg (Unix.error_message error)
  ;;

  let to_dyn (error, syscall, arg) =
    Dyn.Record
      [ "error", String (Unix.error_message error)
      ; "syscall", String syscall
      ; "arg", String arg
      ]
  ;;

  let pp ~prefix unix_error = Pp.verbatim (prefix ^ to_string_hum unix_error)
  let pp_reason unix_error = pp ~prefix:"Reason: " unix_error
  let pp u = pp ~prefix:"" u
end
