open Import

module Cset = struct
  include Cset

  module Range = struct
    type t =
      { first : Char.t
      ; last : Char.t
      }

    let first t = t.first
    let last t = t.last
  end

  let view t =
    fold_right t ~init:[] ~f:(fun first last acc ->
      let range = { Range.first = Cset.to_char first; last = Cset.to_char last } in
      range :: acc)
  ;;
end

module Sem = Automata.Sem
module Rep_kind = Automata.Rep_kind

type t =
  | Set of Cset.t
  | Sequence of Ast.t list
  | Alternative of Ast.t list
  | Repeat of Ast.t * int * int option
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | Beg_of_str
  | End_of_str
  | Last_end_of_line
  | Start
  | Stop
  | Sem of Automata.Sem.t * Ast.t
  | Sem_greedy of Automata.Rep_kind.t * Ast.t
  | Group of string option * Ast.t
  | No_group of Ast.t
  | Nest of Ast.t
  | Case of Ast.t
  | No_case of Ast.t
  | Intersection of Ast.t list
  | Complement of Ast.t list
  | Difference of Ast.t * Ast.t
  | Pmark of Pmark.t * Ast.t

let view_ast f (t : _ Ast.ast) : t =
  match t with
  | Alternative a -> Alternative (List.map ~f a)
  | No_case a -> No_case (f a)
  | Case a -> Case (f a)
;;

let view_set (cset : Ast.cset) : t =
  match cset with
  | Cset set -> Set set
  | Intersection sets -> Intersection (List.map sets ~f:Ast.t_of_cset)
  | Complement sets -> Complement (List.map sets ~f:Ast.t_of_cset)
  | Difference (x, y) -> Difference (Ast.t_of_cset x, Ast.t_of_cset y)
  | Cast ast -> view_ast Ast.t_of_cset ast
;;

let view : Ast.t -> t = function
  | Set s -> view_set s
  | Ast s -> view_ast (fun x -> x) s
  | Sem (sem, a) -> Sem (sem, a)
  | Sem_greedy (sem, a) -> Sem_greedy (sem, a)
  | Sequence s -> Sequence s
  | Repeat (t, x, y) -> Repeat (t, x, y)
  | Beg_of_line -> Beg_of_line
  | End_of_line -> End_of_line
  | Beg_of_word -> Beg_of_word
  | End_of_word -> End_of_word
  | Not_bound -> Not_bound
  | Beg_of_str -> Beg_of_str
  | End_of_str -> End_of_str
  | Last_end_of_line -> Last_end_of_line
  | Start -> Start
  | Stop -> Stop
  | No_group a -> No_group a
  | Group (name, t) -> Group (name, t)
  | Nest t -> Nest t
  | Pmark (pmark, t) -> Pmark (pmark, t)
;;
