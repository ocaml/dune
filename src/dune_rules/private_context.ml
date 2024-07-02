open Import

let t = Build_context.create ~name:(Context_name.of_string "_private")

module Component = struct
  type dev_tool = Ocamlformat

  type t =
    | Lock_dir
    | Dev_tool of dev_tool

  let to_string t =
    match t with
    | Lock_dir -> ".pkg"
    | Dev_tool Ocamlformat -> ".ocamlformat"
  ;;

  let equal t1 t2 = t1 = t2
end
