let langs = ["OCaml"; "Rust"]

let () =
  let s = String.concat ", " langs in
  Format.printf "Hello, %s!\n" s
