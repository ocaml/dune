type t =
  { user : int
  ; group : int
  ; other : int
  ; all : int
  }

let make ~user ~group ~other = { user; group; other; all = user lor group lor other }
let none = make ~user:0 ~group:0 ~other:0
let read = make ~user:0o400 ~group:0o040 ~other:0o004
let execute = make ~user:0o100 ~group:0o010 ~other:0o001
let write = make ~user:0o200 ~group:0o020 ~other:0o002

let ( + ) a b =
  make ~user:(a.user lor b.user) ~group:(a.group lor b.group) ~other:(a.other lor b.other)
;;

module Mode = struct
  type permission = t
  type nonrec t = int

  let of_int t = t
  let to_int t = t

  let create ?(user = none) ?(group = none) ?(other = none) () =
    user.user lor group.group lor other.other
  ;;

  let create_for_all permissions =
    create ~user:permissions ~group:permissions ~other:permissions ()
  ;;

  let default_file = create_for_all (read + write)
  let executable_file = create_for_all (read + write + execute)
  let default_dir = executable_file
  let private_file = create ~user:(read + write) ()
  let add permission t = t lor permission.user
  let test permission t = t land permission.user <> 0
  let test_any permission t = t land permission.all <> 0
  let remove permission t = t land lnot permission.all
end

let add = Mode.add
let test = Mode.test
let test_any = Mode.test_any
let remove = Mode.remove
