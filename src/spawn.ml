module Env = struct
  type t = string array

  let of_array t = t
end

let spawn ?env ~prog ~argv
      ?(stdin=Unix.stdin)
      ?(stdout=Unix.stdout)
      ?(stderr=Unix.stderr)
      () =
  let argv = Array.of_list argv in
  match env with
  | None -> Unix.create_process prog argv stdin stdout stderr
  | Some env -> Unix.create_process_env prog argv env stdin stdout stderr

