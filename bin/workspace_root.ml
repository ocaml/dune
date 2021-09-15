open Stdune
open Dune_engine
open Dune_rules

module Kind = struct
  type t =
    | Explicit
    | Dune_workspace
    | Dune_project
    | Cwd
end

type t =
  { dir : string
  ; to_cwd : string list
  ; reach_from_root_prefix : string
  ; kind : Kind.t
  }

module Candidate = struct
  type t =
    { dir : string
    ; to_cwd : string list
    ; kind : Kind.t
    }
end

let find () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~candidate ~to_cwd dir : Candidate.t option =
    match Sys.readdir dir with
    | exception Sys_error msg ->
      User_warning.emit
        [ Pp.textf
            "Unable to read directory %s. Will not look for root in parent \
             directories."
            dir
        ; Pp.textf "Reason: %s" msg
        ; Pp.text
            "To remove this warning, set your root explicitly using --root."
        ];
      candidate
    | files ->
      let files = String.Set.of_list (Array.to_list files) in
      if String.Set.mem files Workspace.filename then
        Some { kind = Dune_workspace; dir; to_cwd }
      else if String.Set.mem files Dune_project.filename then
        let candidate = Some { Candidate.kind = Dune_project; dir; to_cwd } in
        cont counter ~candidate dir ~to_cwd
      else
        cont counter ~candidate dir ~to_cwd
  and cont counter ~candidate ~to_cwd dir =
    if counter > String.length cwd then
      candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        candidate
      else
        let base = Filename.basename dir in
        loop (counter + 1) parent ~candidate ~to_cwd:(base :: to_cwd)
  in
  loop 0 ~to_cwd:[] cwd ~candidate:None

let create ~specified_by_user =
  match
    match specified_by_user with
    | Some dn -> Some { Candidate.kind = Explicit; dir = dn; to_cwd = [] }
    | None ->
      if Dune_util.Config.inside_dune then
        Some { kind = Cwd; dir = "."; to_cwd = [] }
      else
        find ()
  with
  | Some { Candidate.dir; to_cwd; kind } ->
    { kind
    ; dir
    ; to_cwd
    ; reach_from_root_prefix =
        String.concat ~sep:"" (List.map to_cwd ~f:(sprintf "%s/"))
    }
  | None ->
    User_error.raise
      [ Pp.text "I cannot find the root of the current workspace/project."
      ; Pp.text "If you would like to create a new dune project, you can type:"
      ; Pp.nop
      ; Pp.verbatim "    dune init project NAME"
      ; Pp.nop
      ; Pp.text
          "Otherwise, please make sure to run dune inside an existing project \
           or workspace. For more information about how dune identifies the \
           root of the current workspace/project, please refer to \
           https://dune.readthedocs.io/en/stable/usage.html#finding-the-root"
      ]
