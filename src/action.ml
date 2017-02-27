open Import
open Sexp.Of_sexp

module Mini_shexp = struct
  type 'a t =
    | Run            of 'a * 'a list
    | Chdir          of 'a * 'a t
    | Setenv         of 'a * 'a * 'a t
    | With_stdout_to of 'a * 'a t

  let rec t a sexp =
    sum
      [ cstr_rest "run" (a @> nil) a             (fun prog args -> Run (prog, args))
      ; cstr "chdir"    (a @> t a @> nil)        (fun dn t -> Chdir (dn, t))
      ; cstr "setenv"   (a @> a @> t a @> nil)   (fun k v t -> Setenv (k, v, t))
      ; cstr "with-stdout-to" (a @> t a @> nil)  (fun fn t -> With_stdout_to (fn, t))
      ]
      sexp

  let rec map t ~f =
    match t with
    | Run (prog, args) -> Run (f prog, List.map args ~f)
    | Chdir (fn, t) -> Chdir (f fn, map t ~f)
    | Setenv (var, value, t) -> Setenv (f var, f value, map t ~f)
    | With_stdout_to (fn, t) -> With_stdout_to (f fn, map t ~f)

  let rec fold t ~init:acc ~f =
    match t with
    | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
    | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
    | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f
    | With_stdout_to (fn, t) -> fold t ~init:(f acc fn) ~f

  let to_action ~dir ~env (t : string t) =
    let rec loop vars dir stdouts = function
      | Chdir (fn, t) ->
        loop vars (Path.relative dir fn) stdouts t
      | Setenv (var, value, t) ->
        loop (String_map.add vars ~key:var ~data:value) dir stdouts t
      | With_stdout_to (fn, t) ->
        loop vars dir (Path.relative dir fn :: stdouts) t
      | Run (prog, args) ->
        let stdout_to, touches =
          match stdouts with
          | [] -> None, []
          | p :: rest -> (Some p, rest)
        in
        { Action.
          prog = Path.relative dir prog
        ; args = args
        ; dir
        ; env = Context.extend_env ~vars ~env
        ; stdout_to
        ; touches
        }
    in
    loop String_map.empty dir [] t

  let rec sexp_of_t f : _ -> Sexp.t = function
    | Run (a, xs) -> List (Atom "run" :: f a :: List.map xs ~f)
    | Chdir (a, r) -> List [Atom "chdir" ; f a ; sexp_of_t f r]
    | Setenv (k, v, r) -> List [Atom "setenv" ; f k ; f v ; sexp_of_t f r]
    | With_stdout_to (fn, r) -> List [Atom "with-stdout-to"; f fn; sexp_of_t f r]
end

module T = struct
  type 'a t =
    | Bash of 'a
    | Shexp of 'a Mini_shexp.t

  let t a sexp =
    match sexp with
    | Atom _ -> Bash  (a              sexp)
    | List _ -> Shexp (Mini_shexp.t a sexp)

  let map t ~f =
    match t with
    | Bash x -> Bash (f x)
    | Shexp x -> Shexp (Mini_shexp.map x ~f)

  let fold t ~init ~f =
    match t with
    | Bash x -> f init x
    | Shexp x -> Mini_shexp.fold x ~init ~f

  let sexp_of_t f : _ -> Sexp.t = function
    | Bash a -> List [Atom "bash" ; f a]
    | Shexp a -> List [Atom "shexp" ; Mini_shexp.sexp_of_t f a]
end

include T

module Unexpanded = String_with_vars.Lift(T)

let to_action ~dir ~env = function
  | Shexp shexp -> Mini_shexp.to_action ~dir ~env shexp
  | Bash cmd ->
    { Action.
      prog = Path.absolute "/bin/bash"
    ; args = ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
    ; env
    ; dir
    ; stdout_to = None
    ; touches = []
    }
