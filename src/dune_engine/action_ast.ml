open Import
open Action_types
module Stanza = Dune_lang.Stanza

module type Encoder = sig
  type t

  val encode : t -> Dune_lang.t
end

module Make
    (Program : Encoder)
    (Path : Encoder)
    (Target : Encoder)
    (String : Encoder)
    (Extension : Encoder)
    (Ast : Action_intf.Ast
             with type program := Program.t
             with type path := Path.t
             with type target := Target.t
             with type string := String.t
              and type ext := Extension.t) =
struct
  include Ast

  let rec encode =
    let open Dune_lang in
    let program = Program.encode in
    let string = String.encode in
    let path = Path.encode in
    let target = Target.encode in
    function
    | Run (a, xs) -> List (atom "run" :: program a :: List.map xs ~f:string)
    | With_accepted_exit_codes (pred, t) ->
      List
        [ atom "with-accepted-exit-codes"
        ; Predicate_lang.encode Dune_lang.Encoder.int pred
        ; encode t
        ]
    | Dynamic_run (a, xs) ->
      List (atom "run_dynamic" :: program a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [ atom "chdir"; path a; encode r ]
    | Setenv (k, v, r) -> List [ atom "setenv"; string k; string v; encode r ]
    | Redirect_out (outputs, fn, perm, r) ->
      List
        [ atom
            (sprintf "with-%s-to%s"
               (Outputs.to_string outputs)
               (File_perm.suffix perm))
        ; target fn
        ; encode r
        ]
    | Redirect_in (inputs, fn, r) ->
      List
        [ atom (sprintf "with-%s-from" (Inputs.to_string inputs))
        ; path fn
        ; encode r
        ]
    | Ignore (outputs, r) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode r ]
    | Progn l -> List (atom "progn" :: List.map l ~f:encode)
    | Echo xs -> List (atom "echo" :: List.map xs ~f:string)
    | Cat x -> List [ atom "cat"; path x ]
    | Copy (x, y) -> List [ atom "copy"; path x; target y ]
    | Symlink (x, y) -> List [ atom "symlink"; path x; target y ]
    | Hardlink (x, y) -> List [ atom "hardlink"; path x; target y ]
    | System x -> List [ atom "system"; string x ]
    | Bash x -> List [ atom "bash"; string x ]
    | Write_file (x, perm, y) ->
      List [ atom ("write-file" ^ File_perm.suffix perm); target x; string y ]
    | Rename (x, y) -> List [ atom "rename"; target x; target y ]
    | Remove_tree x -> List [ atom "remove-tree"; target x ]
    | Mkdir x -> List [ atom "mkdir"; path x ]
    | Diff { optional; file1; file2; mode = Binary } ->
      assert (not optional);
      List [ atom "cmp"; path file1; target file2 ]
    | Diff { optional = false; file1; file2; mode = _ } ->
      List [ atom "diff"; path file1; target file2 ]
    | Diff { optional = true; file1; file2; mode = _ } ->
      List [ atom "diff?"; path file1; target file2 ]
    | Merge_files_into (srcs, extras, into) ->
      List
        [ atom "merge-files-into"
        ; List (List.map ~f:path srcs)
        ; List (List.map ~f:string extras)
        ; target into
        ]
    | No_infer r -> List [ atom "no-infer"; encode r ]
    | Pipe (outputs, l) ->
      List
        (atom (sprintf "pipe-%s" (Outputs.to_string outputs))
        :: List.map l ~f:encode)
    | Extension ext -> List [ atom "ext"; Extension.encode ext ]

  let run prog args = Run (prog, args)

  let chdir path t = Chdir (path, t)

  let setenv var value t = Setenv (var, value, t)

  let with_stdout_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stdout, path, perm, t)

  let with_stderr_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stderr, path, perm, t)

  let with_outputs_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Outputs, path, perm, t)

  let with_stdin_from path t = Redirect_in (Stdin, path, t)

  let ignore_stdout t = Ignore (Stdout, t)

  let ignore_stderr t = Ignore (Stderr, t)

  let ignore_outputs t = Ignore (Outputs, t)

  let progn ts = Progn ts

  let echo s = Echo s

  let cat path = Cat path

  let copy a b = Copy (a, b)

  let symlink a b = Symlink (a, b)

  let system s = System s

  let bash s = Bash s

  let write_file ?(perm = File_perm.Normal) p s = Write_file (p, perm, s)

  let rename a b = Rename (a, b)

  let remove_tree path = Remove_tree path

  let mkdir path = Mkdir path

  let diff ?(optional = false) ?(mode = Diff.Mode.Text) file1 file2 =
    Diff { optional; file1; file2; mode }
end
