type expr =
  | Int of int
  | Var of string
  | Succ of expr
  | Constr of string * expr list
  | Let of string * expr * expr
  | Dup of expr * string * string * expr
  | Drop of expr * expr
  | Call of string * expr list

type pattern =
  | PZero
  | PSucc of string
  | PVar of string
  | PWildcard
  | PConstr of string * pattern list

type definition = {
  name : string;
  patterns : pattern list;
  body : expr;
}

type program = {
  definitions : definition list;
  main : expr;
}

let app_to_string item_to_string name items =
  match items with
  | [] -> name
  | _ ->
      Printf.sprintf "%s(%s)" name
        (items |> List.map item_to_string |> String.concat ", ")

let rec expr_to_string = function
  | Int n -> string_of_int n
  | Var name -> name
  | Succ expr -> Printf.sprintf "succ(%s)" (expr_to_string expr)
  | Constr (name, args) -> app_to_string expr_to_string name args
  | Let (name, value, body) ->
      Printf.sprintf "let %s = %s in %s" name (expr_to_string value)
        (expr_to_string body)
  | Dup (value, left, right, body) ->
      Printf.sprintf "dup %s as %s, %s in %s" (expr_to_string value) left
        right (expr_to_string body)
  | Drop (value, body) ->
      Printf.sprintf "drop %s in %s" (expr_to_string value)
        (expr_to_string body)
  | Call (name, args) -> app_to_string expr_to_string name args

let rec pattern_to_string = function
  | PZero -> "0"
  | PSucc name -> Printf.sprintf "succ(%s)" name
  | PVar name -> name
  | PWildcard -> "_"
  | PConstr (name, args) -> app_to_string pattern_to_string name args

let definition_to_string def =
  let head = def.patterns |> List.map pattern_to_string |> String.concat ", " in
  Printf.sprintf "def %s(%s) = %s" def.name head (expr_to_string def.body)

let to_string program =
  match program.definitions with
  | [] -> expr_to_string program.main
  | defs ->
      let defs_text = defs |> List.map definition_to_string |> String.concat "\n" in
      defs_text ^ "\n\n" ^ expr_to_string program.main
