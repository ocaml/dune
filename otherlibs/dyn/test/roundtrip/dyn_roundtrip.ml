let _ : int = 42
let _ : int = -1
let _ : int = 0
let _ : int = 4611686018427387903
let _ : int = -4611686018427387904
let _ : string = "hello"
let _ : string = ""
let _ : string = "with\nnewline"
let _ : string = "with\ttab"
let _ : string = "with \"quotes\""
let _ : string = "with 'single quotes'"
let _ : string = "with\\backslash"
let _ : string = "\000\001\002"
let _ : char = 'a'
let _ : char = '\n'
let _ : char = '\t'
let _ : char = '\r'
let _ : char = '\''
let _ : char = '"'
let _ : char = '\\'
let _ : char = '\000'
let _ : float = 3.14
let _ : float = 0.
let _ : float = -0.
let _ : float = -1.5
let _ : float = 1.7976931348623157e+308
let _ : float = 2.2250738585072014e-308
let _ : float = 1e-300
let _ : float = 1e+300
let _ : bool = true
let _ : bool = false
let _ : unit = ()
let _ : int list = [ 1; 2; 3 ]
let _ : _ list = []
let _ : string list = [ "a"; "b" ]
let _ : bool list = [ true; false; true ]
let _ : int array = [| 1; 2; 3 |]
let _ : _ array = [||]
let _ : string array = [| "x"; "y" |]
let _ : _ option = None
let _ : int option = Some 42
let _ : string option = Some "hello"
let _ : _ option = None
let _ : int * string = 1, "x"
let _ : int * string * bool = 1, "x", true
let _ : unit * unit = (), ()
let _ : int list list = [ [ 1; 2 ]; [ 3 ] ]
let _ : _ list list = [ []; [ 1 ]; [ 2; 3 ] ]
let _ : int option option = Some (Some 1)
let _ : _ option option = Some None
let _ : _ option = None
let _ : int list option = Some [ 1; 2; 3 ]
let _ : _ list option = Some []
let _ : int option list = [ Some 1; None; Some 2 ]
let _ : (int * string) list = [ 1, "a"; 2, "b" ]
let _ : int list * string option = [ 1; 2 ], Some "x"
let _ : int option array = [| Some 1; None |]
let _ : int list list list = [ [ [ 1 ] ]; [ [ 2; 3 ]; [ 4 ] ] ]
let _ : int option option option = Some (Some (Some 42))
let _ : (int * int) * (string * string) = (1, 2), ("a", "b")

type record_0 = { x : int }

let _ : record_0 = { x = 1 }

type record_1 =
  { x : int
  ; y : string
  }

let _ : record_1 = { x = 1; y = "hello" }

type record_2 =
  { name : string
  ; value : bool
  }

let _ : record_2 = { name = "foo"; value = true }

type variant_0 = Foo

let _ : variant_0 = Foo

type variant_1 = Bar of int

let _ : variant_1 = Bar 42

type variant_2 = Baz of int * string

let _ : variant_2 = Baz (1, "x")
