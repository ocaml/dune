module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs (** Both Stdout and Stderr *)
end

module type Ast = sig
  type program
  type path
  type string

  type t =
    | Run            of program * string list
    | Chdir          of path * t
    | Setenv         of string * string * t
    | Redirect       of Outputs.t * path * t
    | Ignore         of Outputs.t * t
    | Progn          of t list
    | Echo           of string
    | Cat            of path
    | Copy           of path * path
    | Symlink        of path * path
    | Copy_and_add_line_directive of path * path
    | System         of string
    | Bash           of string
    | Write_file     of path * string
    | Rename         of path * path
    | Remove_tree    of path
    | Mkdir          of path
    | Digest_files   of path list
end

