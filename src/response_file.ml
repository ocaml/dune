open Import

(* After seeing this number of paths, switch to a response file *)
let max_paths = 4

let too_long spec =
  let add n len =
    let n = n + len in
    if n >= max_paths then raise_notrace Exit;
    n
  in
  let rec loop acc (spec : Arg_spec.Simple.t) =
    match spec with
    | A _ | As _ -> acc
    | Path _ | Dep _ -> add acc 1
    | Paths l | Deps l -> add acc (List.length l)
    | S l -> List.fold_left l ~init:acc ~f:loop
  in
  try
    ignore (loop 0 spec : int);
    false
  with Exit ->
    true

let process sctx ~key ~exec_dir ~response_file_dir ~arg_name
      ~dump_response_file spec =
  match arg_name with
  | None -> Arg_spec.of_simple spec
  | Some arg ->
    if not (Sys.win32 && too_long spec) then
      Arg_spec.of_simple spec
    else begin
      let contents =
        dump_response_file (Arg_spec.Simple.expand spec ~dir:exec_dir)
      in
      let file =
        Path.relative response_file_dir ("." ^ key ^ ".response-file")
      in
      Super_context.add_rule sctx (Build.write_file file contents);
      Arg_spec.S
        [ A arg
        ; Dep file
        ; Hidden_deps (Arg_spec.Simple.deps spec |> Path.Set.to_list)
        ]
    end

let process_ocaml_call sctx ~key ~exec_dir ~response_file_dir spec =
  process sctx ~key ~exec_dir ~response_file_dir spec
    ~arg_name:(Option.some_if
                 (Ocaml_version.supports_response_file
                    (Super_context.context sctx).version)
                 "-args0")
    ~dump_response_file:(fun l ->
      String.concat (List.map l ~f:(fun s -> s ^ "\x00")) ~sep:"")
