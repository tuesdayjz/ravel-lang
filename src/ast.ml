type expr =
  | Int of int
  | Var of string
  | Succ of expr
  | Let of string * expr * expr
  | Dup of expr * string * string * expr
  | Drop of expr * expr
  | Call of string * expr list

type pattern =
  | PZero
  | PSucc of string
  | PVar of string

type definition = {
  name : string;
  pattern : pattern;
  params : string list;
  body : expr;
}

type program = {
  definitions : definition list;
  main : expr;
}

let rec expr_to_string = function
  | Int n -> string_of_int n
  | Var name -> name
  | Succ expr -> Printf.sprintf "succ(%s)" (expr_to_string expr)
  | Let (name, value, body) ->
      Printf.sprintf "let %s = %s in %s" name (expr_to_string value)
        (expr_to_string body)
  | Dup (value, left, right, body) ->
      Printf.sprintf "dup %s as %s, %s in %s" (expr_to_string value) left
        right (expr_to_string body)
  | Drop (value, body) ->
      Printf.sprintf "drop %s in %s" (expr_to_string value)
        (expr_to_string body)
  | Call (name, args) ->
      Printf.sprintf "%s(%s)" name
        (args |> List.map expr_to_string |> String.concat ", ")

let pattern_to_string = function
  | PZero -> "0"
  | PSucc name -> Printf.sprintf "succ(%s)" name
  | PVar name -> name

let definition_to_string def =
  let params = String.concat ", " def.params in
  let head =
    if params = "" then pattern_to_string def.pattern
    else Printf.sprintf "%s, %s" (pattern_to_string def.pattern) params
  in
  Printf.sprintf "def %s(%s) = %s" def.name head (expr_to_string def.body)

let to_string program =
  match program.definitions with
  | [] -> expr_to_string program.main
  | defs ->
      let defs_text = defs |> List.map definition_to_string |> String.concat "\n" in
      defs_text ^ "\n\n" ^ expr_to_string program.main
