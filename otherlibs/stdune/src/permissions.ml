type t =
  { current_user : int
  ; all_users : int
  }

let execute = { current_user = 0o100; all_users = 0o111 }
let write = { current_user = 0o200; all_users = 0o222 }
let add t perm = perm lor t.current_user
let test t perm = perm land t.current_user <> 0
let remove t perm = perm land lnot t.all_users

module Mode = struct
  type permission = t
  type nonrec t = int

  let of_int t = t
  let to_int t = t
  let default_file = of_int 0o666
end
