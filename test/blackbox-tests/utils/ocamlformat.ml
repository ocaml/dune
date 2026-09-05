(* This can be used when testing formatting rules instead of calling the real
   ocamlformat binary. But when doing so, be careful not to expose it to too
   many tests because it will be also be used by `@fmt` in dune itself.
*)

let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let quoted_args =
    args |> List.map (fun s -> Printf.sprintf "%S" s) |> String.concat " "
  in
  Printf.eprintf "fake ocamlformat is running: %s\n" quoted_args;
  let output = "(* fake ocamlformat output *)" in
  if List.mem "--inplace" args
  then (
    let input = List.hd (List.rev args) in
    let oc = open_out_bin input in
    Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc output))
  else Printf.printf "%s" output
;;
