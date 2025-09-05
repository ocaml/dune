let name = "bin3"
let hello name = print_endline ("Hi " ^ name)

let () = Library1.hello name

let () = hello Library1.name
