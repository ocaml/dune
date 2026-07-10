open Stdune

type t =
  | Rpc
  | Gc
  | Alloc
  | Fd
  | Sandbox
  | Persistent
  | Process
  | Rules
  | Pkg
  | Scheduler
  | Promote
  | Build
  | Debug
  | Config
  | File_watcher
  | Diagnostics
  | Log
  | Cram
  | Action
  | Cache
  | Digest
  | Artifact_substitution
  | Thread

let all =
  [ Rpc
  ; Gc
  ; Alloc
  ; Fd
  ; Sandbox
  ; Persistent
  ; Process
  ; Rules
  ; Pkg
  ; Scheduler
  ; Promote
  ; Build
  ; Debug
  ; Config
  ; File_watcher
  ; Diagnostics
  ; Log
  ; Cram
  ; Action
  ; Cache
  ; Digest
  ; Artifact_substitution
  ; Thread
  ]
;;

let to_string = function
  | Rpc -> "rpc"
  | Gc -> "gc"
  | Alloc -> "alloc"
  | Fd -> "fd"
  | Sandbox -> "sandbox"
  | Persistent -> "persistent"
  | Process -> "process"
  | Rules -> "rules"
  | Pkg -> "pkg"
  | Scheduler -> "scheduler"
  | Promote -> "promote"
  | Build -> "build"
  | Debug -> "debug"
  | Config -> "config"
  | File_watcher -> "file_watcher"
  | Diagnostics -> "diagnostics"
  | Log -> "log"
  | Cram -> "cram"
  | Action -> "action"
  | Cache -> "cache"
  | Digest -> "digest"
  | Artifact_substitution -> "artifact_subtitution"
  | Thread -> "thread"
;;

let of_string =
  let all = List.map all ~f:(fun a -> to_string a, a) in
  fun x -> List.assoc_opt x all
;;

let to_dyn t = Dyn.variant (String.uppercase_ascii (to_string t)) []

module Set = Bit_set.Make (struct
    type nonrec t = t

    let to_dyn = to_dyn
    let all = all

    let to_int = function
      | Rpc -> 0
      | Gc -> 1
      | Alloc -> 2
      | Fd -> 3
      | Sandbox -> 4
      | Persistent -> 5
      | Process -> 6
      | Rules -> 7
      | Pkg -> 8
      | Scheduler -> 9
      | Promote -> 10
      | Build -> 11
      | Debug -> 12
      | Config -> 13
      | File_watcher -> 14
      | Diagnostics -> 15
      | Log -> 16
      | Cram -> 17
      | Action -> 18
      | Cache -> 19
      | Digest -> 20
      | Artifact_substitution -> 21
      | Thread -> 22
    ;;
  end)

let default =
  [ Config
  ; Sandbox
  ; Persistent
  ; Process
  ; Rules
  ; Pkg
  ; Promote
  ; Build
  ; Log
  ; File_watcher
  ; Diagnostics
  ; Cram
  ; Action
  ; Artifact_substitution
  ; Thread
  ]
;;

let enabled () =
  let of_string_exn x =
    match of_string x with
    | Some x -> x
    | None -> User_error.raise [ Pp.textf "unrecognized trace category %S" x ]
  in
  let res =
    match Sys.getenv_opt "DUNE_TRACE" with
    | None -> default
    | Some s ->
      let tokens =
        let dune_trace_re = Re.compile (Re.set ",+-") in
        Re.split_full dune_trace_re s
        |> List.map ~f:(function
          | `Text s -> `Category (of_string_exn s)
          | `Delim g ->
            (match Re.Group.get g 0 with
             | "," -> `Comma
             | "+" -> `Add
             | "-" -> `Remove
             | _ -> assert false))
      in
      if
        List.for_all tokens ~f:(function
          | `Category _ | `Comma -> true
          | _ -> false)
      then
        (* We can do better validation here *)
        List.filter_map tokens ~f:(function
          | `Category x -> Some x
          | _ -> None)
      else (
        let rec loop acc = function
          | `Add :: `Category cat :: xs ->
            let acc = cat :: acc in
            loop acc xs
          | `Remove :: `Category cat :: xs ->
            let acc = List.filter acc ~f:(fun x -> x <> cat) in
            loop acc xs
          | [] -> acc
          | _ :: _ ->
            User_error.raise
              [ Pp.text
                  "invalid DUNE_TRACE. Either specify categories by only ',' or a mix of \
                   '+', and '-' "
              ]
        in
        loop default tokens)
  in
  Set.of_list res
;;
