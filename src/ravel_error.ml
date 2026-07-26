type position = { line : int; column : int }

exception Parse_error of position * string
exception Compile_error of string

let string_of_position pos =
  Printf.sprintf "line %d, column %d" pos.line pos.column

let parse_error pos message = raise (Parse_error (pos, message))
let compile_error message = raise (Compile_error message)
