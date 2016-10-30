open StdLabels
open MoreLabels

module String_set = Set.Make(String)

let max_jobs = ref 1

(* Simplified Async/Lwt like monad *)
module Future : sig
  type 'a t


  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  
  module Ivar : sig
    type 'a t
    val fill : 'a t -> 'a -> unit
  end

  val create : ('a Ivar.t -> unit) -> 'a t

  val run : string -> string list -> unit t

  module Scheduler : sig
    val go : 'a t -> 'a
  end
end = struct
  type 'a t = { state : 'a state }

  and 'a state =
    'a state =
    | Return of 'a
    | Sleep of 'a handlers
    | Repr of 'a t

  and 'a handlers =
    | Empty
    | One    of ('a -> unit)
    | Append of 'a handlers * 'a handlers

  let append h1 h2 =
    match h1, h2 with
    | Empty, _ -> h2
    | _, Empty -> h1
    | _ -> Append (h1, h2)

  let rec repr t =
    match t.state with
    | Repr t' -> let t'' = repr t' in if t'' != t' then t.state <- Repr t''; t''
    | _       -> t

  let run_handlers handlers x =
    let rec loop handlers acc =
      match handlers, acc with
      | Empty, [] -> ()
      | Empty, h :: acc -> loop h acc
      | One f, [] -> f x
      | One f, h :: acc -> f x; loop h acc
      | Append (h1, h2) -> loop h1 (h2 :: acc)
    in
    loop handlers []


  let connect t1 t2 =
    let t1 = repr t1 and t2 = repr t2 in
    match t1.state with
    | Sleep h1 ->
      if t1 == t2 then
        ()
      else begin
        match t2.state with
        | Repr _ -> assert false
        | Sleep h2 ->
          t2.state <- Repr t1;
          t1.state <- Sleep (append h1 h2)
        | Return x as state2 ->
          t1.state <- state2;
          run_handlers h1 x
      end
    | _ ->
        assert false

  let return x = { state = Return x }

  let sleeping () = { state = Sleep Empty }

  let ( >>= ) t f =
    let t = repr t in
    match t.state with
    | Return v -> f v
    | Sleep handlers ->
      let res = sleeping () in
      t.state <- Sleep (append handlers (One (fun x -> connect res (f x))));
      res
    | Repr _ ->
      assert false

  let create f =
    let t = sleeping () in
    f t;
    t

  module Ivar = struct
    type nonrec 'a t = 'a t

    let fill t x =
      match state t with
      | Repr _ -> assert false
      | Return _ -> failwith "Ivar.fill"
      | Sleep handlers ->
        t.state <- Return x;
        run_handlers handlers x
  end

  let rec all = function
    | [] -> return []
    | x :: l ->
      x >>= fun x ->
      all l >>= fun l ->
      return (x :: l)

  let rec all_unit = function
    | [] -> return ()
    | x :: l ->
      x >>= fun () ->
      all l

  type job =
    { prog : string
    ; args : string
    ; ivar : unit Ivar.t
    }

  let to_run = Qeueue.create ()

  let run prog args =
    create (fun ivar ->
        Queue.push { prog; args; ivar } to_run)

  module Scheduler = struct
    let process_done { prog; args; ivar } status =
      match status with
      | Unix.WEXITED 0 -> Ivar.fill ivar ()
      | _ ->
        Printf.ksprintf failwith "Process \"%s\" exited with status %s"
          (String.concat (prog :: args) ~sep:" ")
          (match status with
           | WEXITED n -> n
           | WSIGNALED n -> 128 + n
           | WSTOPPED _ -> assert false)

    let running = Hashtbl.create 128

    let rec wait_win32 () =
      let finished =
        Hashtbl.fold running ~init:[] ~f:(fun pid job acc ->
            let pid, status = Unix.waitpid [WNOHANG] pid in
            if pid <> 0 then begin
              process_done job status;
              pid :: acc
            end else
              acc)
      in
      List.iter finished ~f:(Hashtbl.remove running)

    let go t =
      match (repr t).state with
      | Return v -> v
      | _ ->
        while Hashtbl.length running < !max_jobs && not (Queue.is_empty to_run) do
          let job = Queue.pop to_run in
          let pid =
            Unix.create_process job.prog (Array.of_list (job.prog :: job.args))
              Unix.stdin Unix.stdout Unix.stderr
          in
          Hashtbl.add running pid job
        done;
        if Sys.win32 then
          wait_win32 ()
        else begin
          let pid, status = Unix.wait () in
          process_done (Hashtbl.find running pid) status;
          Hashtbl.remove running pid
        end;
        go t
  end
end
open Future

type ('a, 'b) eq =
  | Eq : ('a, 'a) eq
  | Ne : ('a, 'b) eq

module Kind = struct
  type 'a t =
    | Strings : string list t

  let eq : type a b. a t -> b t -> (a, b) eq = fun a b ->
    match a, b with
    | File, File -> Eq
    | Strings, Strings -> Eq
    | _ -> Ne
end

module Vals = struct
  type 'a t =
    | []     : unit t
    | ( :: ) : 'a * 'b t -> ('a -> 'b) t
end

module Vals_spec = struct
  type 'a t =
    | []     : unit t
    | ( :: ) : (string * 'a Kind.t) * 'b t -> ('a -> 'b) t
end

(* dep/target specification *)
module Spec = struct
  type _ t =
    | Files : string list -> unit t
    | Vals  : 'a Vals_spec.t -> 'a Vals.t t
    | Both  : string list * 'a Vals_spec.t -> 'a Vals.t t

  let to_files_and_vals : type a. a t -> string list * a Vals_spec.t = function
    | Files l -> (l, [])
    | Vals l -> ([], l)
    | Both (f, v) -> (f, v)
end

module Rule : sig
  val rule
    :  deps:'a Spec.t
    -> targets:'b Spec.t
    -> ('a -> 'b Lwt.t)
    -> unit
end = struct
  type t =
    { deps    : string list
    ; targets : string list
    ; exec    : unit Lwt.t Lazy.t
    }

  type value_cell =
      V : { rule         : t (* Rule which produces it *)
          ; kind         : 'a Kind.t
          ; mutable data : 'a option
          } -> value_cell

  type packed_value_cell = V : _ value_cell -> packed_value_cell

  let values = Hashtbl.create 1024
  let files  = Hashtbl.create 1024

  let rec wait_for_value path kind =
    let (V v) = Hashtbl.find values path in
    match Kind.eq kind v.kind with
    | Ne -> assert false
    | Eq ->
      Lazy.force rule.exec >>= fun () ->
      match v.data with
      | Some x -> return x
      | None -> assert false

  let wait_for_values : type a. a Vals_spec.t -> a Vals.t =
    let open Vals_spec in
    function
    | [] -> return []
    | (path, kind) :: spec ->
      let rest = wait_for_values spec in
      wait_for_value path kind >>= fun x ->
      rest >>= l ->
      return (x :: l)

  let wait_for_file path =
    match Hashtbl.find files path with
    | exception Not_found ->
      if Sys.file_exists path then
        return ()
    | rule -> Lazy.force rule.exec

  let store path kind x =
    let (V v) = Hashtbl.find values path in
    match Kind.eq kind v.kind with
    | Ne -> assert false
    | Eq -> v.data <- Some x

  let store_all : type a. a Vals_spec.t -> a Vals.t -> unit =
    let open Vals_spec in
    let open Vals in
    fun spec vals ->
      match spec, vals with
      | [], [] -> ()
      | (path, kind) :: spec, x :: vals ->
        store path kind x;
        store_all spec vals

  let create_value_cells : type a. a Vals_spec.t -> t -> unit =
    let open Vals_spec in
    fun spec rule ->
      match spec with
      | [] -> ()
      | (path, kind) :: spec ->
        Hashtbl.add values path { kind; rule; data = None };
        create_value_cells spec rule
  
  let rule ~deps ~targets f =
    let fdeps   , vdeps    = Spec.to_files_and_vals deps    in
    let ftargets, vtargets = Spec.to_files_and_vals targets in
    let exec = lazy (
      Future.all_unit (List.map fdeps ~f:wait_for_file) >>= fun () ->
      wait_for_values vdeps >>= fun vals ->
      f vals >>= fun results ->
      store_all vtargets results;
      return ()
    ) in
    let rule = { deps; targets; exec } in
    List.iter ftargets ~f:(fun fn -> Hashtbl.add files fn rule);
    create_value_cells vtargets rule
end

module Of_sexp = struct
  module Field_spec = struct
    type 'a t =
      { name : string
      ; of_sexp : Sexp.t -> 'a
      ; default : 'a option
      }
  end

  module Spec = struct

  end
end


module Lib = struct
  type t =
    { name        : string
    ; public_name : string option
    ; libraries   : string list
    ; modules     : String_set.t
    ; c_flags     : string list
    ; c_names     : string list
    }

  let guess_modules ~dir ~files_produced_by_rules =
    Sys.readdir dir
    |> Array.to_list
    |> List.append files_produced_by_rules
    |> List.filter ~f:(fun fn ->
        Filename.check_extension fn ".mli"
        || Filename.check_extension fn ".ml")
    |> List.map ~f:(fun fn ->
        String.capitalize (Filename.chop_extension fn))
    |> String_set.of_list

  let parse ~dir ~files_produced_by_rules sexp =
    Of_sexp.parse sexp
      [ field   "name"        string
      ; field_o "public_name" string
      ; field   "libraries"   (list string) ~default:[]
      ; field_o "modules"     string_set
      ; field   "c_flags"     (list string) ~default:[]
      ; field   "c_names"     (list string) ~default:[]
      ]
      (fun name public_name libraries modules c_flags c_names ->
         let modules =
           match modules with
           | None ->
             guess_modules ~dir ~files_produced_by_rules
           | Some x -> x
         in
         { name
         ; public_name
         ; libraries
         ; modules
         ; c_flags
         ; c_names
         })

  let setup_rules ~dir t =
    let pped_files =
      List.map t.modules ~f:(fun m ->
          dir ^/ String.uncapitalize m ^ ".pp")
    in
    let source_deps = (sprintf "ocamldep for %s" t.name, Kind.Strings) in
    let depends_fn = dir ^/ ".depends" in
    rule ~deps:(Files pped_files) ~targets:(Files [depends_fn]) (fun () ->
         run ~stdout_to:depends_fn "ocamldep" pped_files);
    rule ~deps:(Files [depends_fn]) ~targets:(Vals [source_deps]) (fun () ->
        (* parse *)
        return [deps]);
    List.iter t.modules ~f:(fun m ->
        let src = dir ^/ String.uncapitalize m ^ ".ml" in
        let dst = dir ^/ t.name ^ "__" ^ m ^ ".cmo" in
        rule ~deps:(Both (src, [source_deps])) ~targets:(Files [dst])
          (fun deps ->
             List.iter (String_map.find deps m) ~f:(fun m -> wait_for_file (... ^ m ^ ".cmi")) >>= fun () ->
             run "ocamlc" ["-c"; src]);
        
end

module Rule = struct
  type t =
    { targets : string list
    ; deps    : string list
    ; action  : string
    }

  let parse sexp =
    Of_sexp.parse
      [ field "targets" (string list)
      ; field "deps"    (string list)
      ; field "action"  string
      ]
      (fun targets deps action ->
         { targets; deps; action })
end

module Jbuild = struct
  type t =
    | Library of Lib.t
    | Rule    of Rule.t

  let parse ~dir sexps =
    let rules =
      List.filter_map sexps ~f:(function
          | List [Atom "rule"; arg] ->
            Some (Rule.parse arg)
          | _ -> None)
    in
    let files_produced_by_rules =
      List.concat_map rules ~f:(fun r -> r.targets)
    in
    List.filter_map sexps ~f:(function
        | List [Atom "library"; arg] ->
          Some (Library (Lib.parse ~dir ~files_produced_by_rules))
        | _ ->
          None)

  let load ~dir =
    let fn = dir ^/ "jbuild" in
    let jbuilds = Lexer.jbuilds () in
end
