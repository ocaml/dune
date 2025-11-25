(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

let doc = "Command group related to Rocq."
let sub_commands_synopsis = Common.command_synopsis [ "rocq top FILE -- ARGS" ]
let man = [ `Blocks sub_commands_synopsis ]
let info = Cmd.info ~doc ~man "rocq"
let group = Cmd.group info [ Rocqtop.command ]
