let langs = ["OCaml"; "Rust"]

let () =
  let pp_langs = Fmt.(list ~sep:(any ", ") string) in
  Format.printf "Hello, %a!\n" pp_langs langs
