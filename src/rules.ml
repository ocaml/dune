open! Stdune

type rule = unit -> unit

module T = struct
  type t = rule Path.Build.Map.t

  let empty = Path.Build.Map.empty

  let union_map a b ~f =
    Path.Build.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union =
    union_map ~f:(fun rule1 rule2 -> fun () -> rule1 (); rule2 ())

  let name = "Rules"
end

include T

let file_rule ~rule:(dst, rule) =
  let dst = Path.as_in_build_dir_exn dst in
  Path.Build.Map.singleton (Path.Build.parent_exn dst) rule

let dir_rule (dir, rule) =
  let dir = Path.as_in_build_dir_exn dir in
  Path.Build.Map.singleton dir rule

let implicit_output = Memo.Implicit_output.add(module T)

let file_rule ~rule =
  Memo.Implicit_output.produce implicit_output (file_rule ~rule)

let dir_rule arg =
  Memo.Implicit_output.produce implicit_output (dir_rule arg)

let collect f =
  let result, out = Memo.Implicit_output.collect_sync implicit_output f in
  result, Option.value out ~default:T.empty

let to_map x = x
