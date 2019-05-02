open Import

module Kind = struct
  type t = Git | Hg

  let of_dir_contents files =
    if String.Set.mem files ".git" then
      Some Git
    else if String.Set.mem files ".hg" then
      Some Hg
    else
      None

  let to_dyn t =
    Dyn.Variant
      ((match t with
         | Git -> "Git"
         | Hg -> "Hg"),
       [])
end

type t =
  { root : Path.t
  ; kind : Kind.t
  }

let to_dyn { root; kind } =
  Dyn.Encoder.record
    [ "root", Path.to_dyn root
    ; "kind", Kind.to_dyn kind
    ]

let git, hg =
  let get prog = lazy (
    match Bin.which ~path:(Env.path Env.initial) prog with
    | Some x -> x
    | None -> Utils.program_not_found prog ~loc:None)
  in
  (get "git", get "hg")

let git_describe =
  Memo.create
    "git-describe"
    ~doc:"Run [git describe] in the following directory"
    ~input:(module Path)
    ~output:(Simple (module String))
    ~visibility:(Public Path_dune_lang.decode)
    Async
    (Some (fun dir ->
       let open Fiber.O in
       let+ s =
         Process.run_capture Strict (Lazy.force git)
           ["describe"; "--always"; "--dirty"] ~env:Env.initial ~dir
       in
       String.trim s))

let hg_describe =
  let f dir =
    let open Fiber.O in
    let hg = Lazy.force hg in
    let hg args = Process.run_capture Strict hg ~env:Env.initial ~dir args in
    let* s =
      hg [ "log"; "--rev"; "."; "-T"; "{latesttag} {latesttagdistance}" ]
    in
    let+ id =
      hg [ "id"; "-i" ]
    in
    let s = String.trim s and id = String.trim id in
    let id, dirty_suffix =
      match String.drop_suffix id ~suffix:"+" with
      | Some id -> id, "-dirty"
      | None -> id, ""
    in
    let s =
      let s, dist = Option.value_exn (String.rsplit2 s ~on:' ') in
      match s with
      | "null" -> id
      | _ ->
        match int_of_string dist with
        | 1 -> s
        | n -> sprintf "%s-%d-%s" s (n - 1) id
        | exception _ -> sprintf "%s-%s-%s" s dist id
    in
    s ^ dirty_suffix
  in
  Memo.create
    "hg-describe"
    ~doc:"Do something similar to [git describe] with hg"
    ~input:(module Path)
    ~output:(Simple (module String))
    ~visibility:(Public Path_dune_lang.decode)
    Async
    (Some f)

let describe { root; kind } =
  match kind with
  | Git -> Memo.exec git_describe root
  | Hg -> Memo.exec hg_describe root
