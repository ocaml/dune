open Import

module File = struct
  type t =
    { path : string
    ; contents : string
    }
end

let _pid_of_path path =
  Filename.basename path
  |> Filename.chop_suffix_opt ~suffix:".pid"
  |> Option.bind ~f:Int.of_string
;;

module Dune = struct
  module T = struct
    type t =
      { root : string
      ; pid : int
      ; where : Where.t
      }

    let compare t { root; pid; where } =
      let open Ordering.O in
      let= () = Int.compare t.pid pid in
      let= () = String.compare t.root root in
      Where.compare t.where where
    ;;

    let to_dyn { root; pid; where } =
      let open Dyn in
      record [ "root", string root; "pid", int pid; "where", Where.to_dyn where ]
    ;;
  end

  include T
  module C = Comparable.Make (T)
  module Set = C.Set

  let create ~where ~root ~pid = { where; root; pid }
  let root t = t.root
  let where t = t.where
  let pid t = t.pid
  let filename dune = Printf.sprintf "%d.csexp" dune.pid

  let sexp : t Conv.value =
    let open Conv in
    let to_ (where, root, pid) = { where; root; pid } in
    let from { where; root; pid } = where, root, pid in
    let where = field "where" (required Where.sexp) in
    let root = field "root" (required string) in
    let pid = field "pid" (required int) in
    iso (record (three where root pid)) to_ from
  ;;

  type error =
    | Of_sexp of Conv.error
    | Csexp of
        { position : int
        ; message : string
        }

  exception E of error

  let () =
    Printexc.register_printer (function
      | E (Of_sexp e) -> Some (Dyn.to_string (Conv.dyn_of_error e))
      | E (Csexp { position; message }) ->
        Some
          (Dyn.to_string
             (let open Dyn in
              record [ "message", string message; "position", int position ]))
      | _ -> None)
  ;;

  let of_file (f : File.t) =
    match Csexp.parse_string f.contents with
    | Error (position, message) -> Error (Csexp { position; message })
    | Ok s ->
      (match Conv.of_sexp sexp ~version:(0, 0) s with
       | Ok s -> Ok s
       | Error e -> Error (Of_sexp e))
  ;;
end

module Config = struct
  type t = Xdg.t

  let watch_dir t =
    let dir =
      match Xdg.runtime_dir t with
      | Some runtime -> runtime
      | None -> Xdg.data_dir t
    in
    Filename.concat dir "dune/rpc"
  ;;

  let create x = x

  let register t dune =
    let file =
      let contents = Conv.to_sexp Dune.sexp dune |> Csexp.to_string in
      let path = Filename.concat (watch_dir t) (Dune.filename dune) in
      { File.contents; path }
    in
    `Caller_should_write file
  ;;
end

type nonrec t =
  { config : Config.t
  ; mutable last_mtime : float option
  ; mutable current : Dune.t list
  }

let create config = { config; last_mtime = None; current = [] }
let current t = t.current

module Refresh = struct
  type t =
    { added : Dune.t list
    ; removed : Dune.t list
    ; errored : (string * exn) list
    }

  let empty = { added = []; removed = []; errored = [] }
  let added t = t.added
  let removed t = t.removed
  let errored t = t.errored
end

module Poll
    (Fiber : sig
       type 'a t

       val return : 'a -> 'a t
       val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

       module O : sig
         val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
         val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
       end
     end)
    (IO : sig
       val scandir : string -> (string list, exn) result Fiber.t
       val stat : string -> ([ `Mtime of float ], exn) result Fiber.t
       val read_file : string -> (string, exn) result Fiber.t
     end) =
struct
  open Fiber.O

  let ( let** ) x f =
    let* x = x in
    match x with
    | Ok s -> f s
    | Error e -> Fiber.return (Error e)
  ;;

  let ( let++ ) x f =
    let+ x = x in
    match x with
    | Ok s -> Ok (f s)
    | Error e -> Error e
  ;;

  let poll t =
    let dir = Config.watch_dir t.config in
    let** (`Mtime mtime) = IO.stat dir in
    let skip =
      match t.last_mtime with
      | Some last_mtime -> last_mtime >= mtime
      | None ->
        t.last_mtime <- Some mtime;
        false
    in
    if skip
    then Fiber.return (Ok Refresh.empty)
    else
      let++ results =
        let** contents = IO.scandir dir in
        let contents =
          List.filter contents ~f:(fun fname -> fname <> "." && fname <> "..")
        in
        let+ res =
          Fiber.parallel_map contents ~f:(fun fname ->
            let path = Filename.concat dir fname in
            let+ contents = IO.read_file path in
            ( path
            , match contents with
              | Error e -> Error e
              | Ok contents ->
                let file = { File.contents; path } in
                (match Dune.of_file file with
                 | Ok _ as s -> s
                 | Error e -> Error (Dune.E e)) ))
        in
        Ok res
      in
      let new_current, errored =
        List.partition_map results ~f:(fun (file, res) ->
          match res with
          | Ok s -> Left s
          | Error e -> Right (file, e))
      in
      let current = t.current in
      t.current <- new_current;
      let module Set = Dune.Set in
      let new_current = Set.of_list new_current in
      let current = Set.of_list current in
      { Refresh.added = Set.to_list (Set.diff new_current current)
      ; removed = Set.to_list (Set.diff current new_current)
      ; errored
      }
  ;;
end
