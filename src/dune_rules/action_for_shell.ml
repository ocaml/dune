open Import
module Engine_action = Action

module Encoding = struct
  type t =
    | Rules
    | Replay
end

let encode_as encoding =
  let module Outputs = Dune_lang.Action.Outputs in
  let module File_perm = Dune_lang.Action.File_perm in
  let module Inputs = Dune_lang.Action.Inputs in
  let open Dune_lang in
  let string = Encoder.string in
  let path = Encoder.string in
  let target = Encoder.string in
  let program = function
    | Engine_action.For_shell.Program.Resolved program -> Encoder.string program
    | Unresolved { context; program; hint } ->
      (match encoding with
       | Encoding.Rules -> Encoder.string (Filename.to_string program)
       | Replay ->
         List
           (atom "unresolved-program"
            :: Encoder.string (Context_name.to_string context)
            :: Encoder.string (Filename.to_string program)
            ::
            (match hint with
             | None -> []
             | Some hint -> [ Encoder.string hint ])))
  in
  let text_diff_name ~optional ~directory_diffs =
    match encoding with
    | Encoding.Rules -> if optional then "diff?" else "diff"
    | Replay ->
      (match optional, directory_diffs with
       | false, true -> "diff"
       | true, true -> "diff?"
       | false, false -> "diff-no-directory"
       | true, false -> "diff-no-directory?")
  in
  let rec encode : Engine_action.For_shell.t -> Dune_lang.t = function
    | Run { prog; args; can_run_in_action_runner = _ } ->
      List
        (atom "run" :: program prog :: List.map (Appendable_list.to_list args) ~f:string)
    | With_accepted_exit_codes (pred, action) ->
      List
        [ atom "with-accepted-exit-codes"
        ; Predicate_lang.encode Dune_sexp.Encoder.int pred
        ; encode action
        ]
    | Chdir (dir, action) -> List [ atom "chdir"; path dir; encode action ]
    | Setenv (var, value, action) ->
      List [ atom "setenv"; string var; string value; encode action ]
    | Redirect_out (outputs, file, perm, action) ->
      List
        [ atom
            (sprintf "with-%s-to%s" (Outputs.to_string outputs) (File_perm.suffix perm))
        ; target file
        ; encode action
        ]
    | Redirect_in (inputs, file, action) ->
      List
        [ atom (sprintf "with-%s-from" (Inputs.to_string inputs))
        ; path file
        ; encode action
        ]
    | Ignore (outputs, action) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode action ]
    | Progn actions -> List (atom "progn" :: List.map actions ~f:encode)
    | Concurrent actions -> List (atom "concurrent" :: List.map actions ~f:encode)
    | Echo strings -> List (atom "echo" :: List.map strings ~f:string)
    | Cat paths -> List (atom "cat" :: List.map paths ~f:path)
    | Copy (source, target_) -> List [ atom "copy"; path source; target target_ ]
    | Symlink (source, target_) -> List [ atom "symlink"; path source; target target_ ]
    | Hardlink (source, target_) -> List [ atom "hardlink"; path source; target target_ ]
    | System command -> List [ atom "system"; string command ]
    | Bash { script; can_run_in_action_runner = _ } -> List [ atom "bash"; string script ]
    | Write_file (target_, perm, contents) ->
      List
        [ atom ("write-file" ^ File_perm.suffix perm); target target_; string contents ]
    | Rename (source, target_) -> List [ atom "rename"; target source; target target_ ]
    | Remove_tree target_ -> List [ atom "remove-tree"; target target_ ]
    | Mkdir target_ -> List [ atom "mkdir"; target target_ ]
    | Pipe (outputs, actions) ->
      List
        (atom (sprintf "pipe-%s" (Outputs.to_string outputs))
         :: List.map actions ~f:encode)
    | Diff { optional; file1; file2; mode = Binary; directory_diffs = _ } ->
      assert (not optional);
      List [ atom "cmp"; path file1; target file2 ]
    | Diff { optional; file1; file2; mode = Text; directory_diffs } ->
      List [ atom (text_diff_name ~optional ~directory_diffs); path file1; target file2 ]
    | Extension extension ->
      List [ atom "ext"; Dune_sexp.Quoted_string (Sexp.to_string extension) ]
  in
  encode
;;

let encode_for_rules = encode_as Encoding.Rules

module Replay = struct
  let encode = encode_as Encoding.Replay

  let decode =
    let open Dune_lang.Decoder in
    let program =
      enter
      @@ sum
           [ ( "unresolved-program"
             , let+ context = string
               and+ program = string
               and+ hint = repeat string in
               let hint =
                 match hint with
                 | [] -> None
                 | [ hint ] -> Some hint
                 | _ ->
                   User_error.raise
                     [ Pp.text
                         "An unresolved-program accepts at most one diagnostic hint."
                     ]
               in
               let program =
                 match Filename.of_string program with
                 | Some program -> program
                 | None ->
                   User_error.raise
                     [ Pp.text "Invalid program name in unresolved-program." ]
               in
               Engine_action.For_shell.Program.Unresolved
                 { context = Context_name.of_string context; program; hint } )
           ]
      <|> (string >>| fun program -> Engine_action.For_shell.Program.Resolved program)
    in
    let run =
      let+ prog = program
      and+ args = repeat string in
      Engine_action.For_shell.Run
        { prog; args = Appendable_list.of_list args; can_run_in_action_runner = false }
    in
    fix
    @@ fun action ->
    let unary make = string >>| make in
    let binary make =
      let+ first = string
      and+ second = string in
      make first second
    in
    let scoped make =
      let+ value = string
      and+ action = action in
      make value action
    in
    let wrapped make = action >>| make in
    let sequence make = repeat action >>| make in
    let redirect_out outputs perm =
      scoped (fun path action ->
        Engine_action.For_shell.Redirect_out (outputs, path, perm, action))
    in
    let diff ~optional ~mode ~directory_diffs =
      binary (fun file1 file2 ->
        Engine_action.For_shell.Diff { optional; mode; file1; file2; directory_diffs })
    in
    sum
      [ "run", run
      ; ( "with-accepted-exit-codes"
        , let+ codes = Predicate_lang.decode_one int
          and+ action = action in
          Engine_action.For_shell.With_accepted_exit_codes (codes, action) )
      ; "chdir", scoped (fun dir action -> Engine_action.For_shell.Chdir (dir, action))
      ; ( "setenv"
        , let+ var = string
          and+ value = string
          and+ action = action in
          Engine_action.For_shell.Setenv (var, value, action) )
      ; "with-stdout-to", redirect_out Stdout Normal
      ; "with-stdout-to-executable", redirect_out Stdout Executable
      ; "with-stderr-to", redirect_out Stderr Normal
      ; "with-stderr-to-executable", redirect_out Stderr Executable
      ; "with-outputs-to", redirect_out Outputs Normal
      ; "with-outputs-to-executable", redirect_out Outputs Executable
      ; ( "with-stdin-from"
        , scoped (fun path action ->
            Engine_action.For_shell.Redirect_in (Stdin, path, action)) )
      ; ( "ignore-stdout"
        , wrapped (fun action -> Engine_action.For_shell.Ignore (Stdout, action)) )
      ; ( "ignore-stderr"
        , wrapped (fun action -> Engine_action.For_shell.Ignore (Stderr, action)) )
      ; ( "ignore-outputs"
        , wrapped (fun action -> Engine_action.For_shell.Ignore (Outputs, action)) )
      ; "progn", sequence (fun actions -> Engine_action.For_shell.Progn actions)
      ; "concurrent", sequence (fun actions -> Engine_action.For_shell.Concurrent actions)
      ; ("echo", repeat string >>| fun strings -> Engine_action.For_shell.Echo strings)
      ; ("cat", repeat string >>| fun paths -> Engine_action.For_shell.Cat paths)
      ; ( "copy"
        , binary (fun source target -> Engine_action.For_shell.Copy (source, target)) )
      ; ( "symlink"
        , binary (fun source target -> Engine_action.For_shell.Symlink (source, target)) )
      ; ( "hardlink"
        , binary (fun source target -> Engine_action.For_shell.Hardlink (source, target))
        )
      ; "system", unary (fun command -> Engine_action.For_shell.System command)
      ; ( "bash"
        , unary (fun script ->
            Engine_action.For_shell.Bash { script; can_run_in_action_runner = false }) )
      ; ( "write-file"
        , binary (fun target contents ->
            Engine_action.For_shell.Write_file (target, Normal, contents)) )
      ; ( "write-file-executable"
        , binary (fun target contents ->
            Engine_action.For_shell.Write_file (target, Executable, contents)) )
      ; ( "rename"
        , binary (fun source target -> Engine_action.For_shell.Rename (source, target)) )
      ; "remove-tree", unary (fun path -> Engine_action.For_shell.Remove_tree path)
      ; "mkdir", unary (fun path -> Engine_action.For_shell.Mkdir path)
      ; ( "pipe-stdout"
        , sequence (fun actions -> Engine_action.For_shell.Pipe (Stdout, actions)) )
      ; ( "pipe-stderr"
        , sequence (fun actions -> Engine_action.For_shell.Pipe (Stderr, actions)) )
      ; ( "pipe-outputs"
        , sequence (fun actions -> Engine_action.For_shell.Pipe (Outputs, actions)) )
      ; "diff", diff ~optional:false ~mode:Text ~directory_diffs:true
      ; "diff?", diff ~optional:true ~mode:Text ~directory_diffs:true
      ; "diff-no-directory", diff ~optional:false ~mode:Text ~directory_diffs:false
      ; "diff-no-directory?", diff ~optional:true ~mode:Text ~directory_diffs:false
      ; "cmp", diff ~optional:false ~mode:Binary ~directory_diffs:false
      ; ( "ext"
        , let+ (_ : string) = string in
          User_error.raise
            [ Pp.text "Edited extension actions cannot be replayed."
            ; Pp.text
                "Restore action.sexp and edit the underlying dune file instead; action \
                 extensions do not provide an inverse decoder."
            ] )
      ]
  ;;
end
