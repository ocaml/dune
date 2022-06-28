open Import

module Query = struct
  type t =
    | Libs of string
    | Cflags of string

  let file t ~dir =
    let dir = Path.Build.relative dir ".pkg-config" in
    Path.Build.relative dir
    @@
    match t with
    | Libs s -> sprintf "%s.libs" s
    | Cflags s -> sprintf "%s.cflags" s

  let to_args t : _ Command.Args.t list =
    Hidden_deps Dep.(Set.singleton universe)
    ::
    (match t with
    | Libs lib -> [ A "--libs"; A lib ]
    | Cflags lib -> [ A "--cflags"; A lib ])

  let default = function
    | Libs lib -> [ sprintf "-l%s" lib ]
    | Cflags _ -> [ "-I/usr/include" ]

  let read t sctx ~dir =
    let open Action_builder.O in
    let* bin =
      Action_builder.of_memo
      @@ Super_context.resolve_program sctx ~loc:None ~dir "pkg-config"
    in
    match bin with
    | Error _ -> Action_builder.return (default t)
    | Ok _ ->
      let file = file t ~dir in
      let+ contents = Action_builder.contents (Path.build file) in
      String.split_lines contents
      |> List.hd |> String.extract_blank_separated_words
end

let gen_rule sctx ~loc ~dir query =
  let open Memo.O in
  let* bin =
    Super_context.resolve_program sctx ~loc:(Some loc) ~dir "pkg-config"
  in
  match bin with
  | Error _ -> Memo.return @@ Error `Not_found
  | Ok _ as bin ->
    let command =
      Command.run ~dir:(Path.build dir) ~stdout_to:(Query.file ~dir query) bin
        (Query.to_args query)
    in
    let+ () = Super_context.add_rule sctx ~loc ~dir command in
    Ok ()

let read_flags ~file =
  let open Action_builder.O in
  let+ contents = Action_builder.contents (Path.build file) in
  String.split_lines contents |> List.hd |> String.extract_blank_separated_words
