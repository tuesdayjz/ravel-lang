type state = { tokens : Token.t array; mutable index : int }

let current st = st.tokens.(st.index)
let current_kind st = (current st).Token.kind
let current_pos st = (current st).Token.start_pos

let advance st =
  if st.index < Array.length st.tokens - 1 then st.index <- st.index + 1

let expect st expected message =
  if expected (current_kind st) then advance st
  else Ravel_error.parse_error (current_pos st) message

let expect_ident st message =
  match current_kind st with
  | Token.TIdent name ->
      advance st;
      name
  | _ -> Ravel_error.parse_error (current_pos st) message

let matches_lparen = function Token.TLParen -> true | _ -> false

let is_constructor_name name =
  String.length name > 0
  &&
  match name.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false

let expect_constructor_name st description =
  let name = expect_ident st (Printf.sprintf "expected %s" description) in
  if not (is_constructor_name name) then
    Ravel_error.parse_error (current_pos st)
      (Printf.sprintf "%s '%s' must begin with an uppercase letter" description name);
  name

let rec parse_nonempty_args st =
  let rec loop acc =
    let expr = parse_expr st in
    match current_kind st with
    | Token.TComma ->
        advance st;
        loop (expr :: acc)
    | Token.TRParen -> List.rev (expr :: acc)
    | _ ->
        Ravel_error.parse_error (current_pos st)
          "expected ',' or ')' in argument list"
  in
  loop []

and parse_args st =
  match current_kind st with
  | Token.TRParen -> []
  | _ -> parse_nonempty_args st

and parse_parenthesized_args st =
  expect st matches_lparen "expected '(' before argument list";
  let args = parse_args st in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after argument list";
  args

and parse_succ_expr st =
  expect st (function Token.TSucc -> true | _ -> false) "expected 'succ'";
  expect st (function Token.TLParen -> true | _ -> false)
    "expected '(' after 'succ'";
  let expr = parse_expr st in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after succ(...)";
  Ast.Succ expr

and parse_expr st =
  match current_kind st with
  | Token.TLet -> parse_let st
  | Token.TDup -> parse_dup st
  | Token.TDrop -> parse_drop st
  | Token.TFun -> parse_lambda st
  | _ -> parse_postfix st

and parse_let st =
  expect st (function Token.TLet -> true | _ -> false) "expected 'let'";
  let name = expect_ident st "expected identifier after 'let'" in
  expect st (function Token.TEqual -> true | _ -> false)
    "expected '=' after binding name";
  let value = parse_expr st in
  expect st (function Token.TIn -> true | _ -> false)
    "expected 'in' after let-bound expression";
  let body = parse_expr st in
  Ast.Let (name, value, body)

and parse_dup st =
  expect st (function Token.TDup -> true | _ -> false) "expected 'dup'";
  let value = parse_expr st in
  expect st (function Token.TAs -> true | _ -> false)
    "expected 'as' after duplicated expression";
  let left = expect_ident st "expected first duplication binder" in
  expect st (function Token.TComma -> true | _ -> false)
    "expected ',' between duplication binders";
  let right = expect_ident st "expected second duplication binder" in
  expect st (function Token.TIn -> true | _ -> false)
    "expected 'in' after duplication binders";
  let body = parse_expr st in
  Ast.Dup (value, left, right, body)

and parse_drop st =
  expect st (function Token.TDrop -> true | _ -> false) "expected 'drop'";
  let value = parse_expr st in
  expect st (function Token.TIn -> true | _ -> false)
    "expected 'in' after dropped expression";
  let body = parse_expr st in
  Ast.Drop (value, body)

and parse_lambda st =
  expect st (function Token.TFun -> true | _ -> false) "expected 'fun'";
  expect st matches_lparen "expected '(' after 'fun'";
  let params =
    match current_kind st with
    | Token.TRParen -> []
    | _ ->
        let rec loop acc =
          let param = expect_ident st "expected lambda parameter" in
          match current_kind st with
          | Token.TComma ->
              advance st;
              loop (param :: acc)
          | Token.TRParen -> List.rev (param :: acc)
          | _ ->
              Ravel_error.parse_error (current_pos st)
                "expected ',' or ')' in lambda parameter list"
        in
        loop []
  in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after lambda parameters";
  expect st (function Token.TEqual -> true | _ -> false)
    "expected '=' after lambda parameters";
  Ast.Lambda (params, parse_expr st)

and parse_postfix st =
  let rec loop callee =
    match current_kind st with
    | Token.TLParen -> loop (Ast.Apply (callee, parse_parenthesized_args st))
    | _ -> callee
  in
  loop (parse_atom st)

and parse_atom st =
  match current_kind st with
  | Token.TInt n ->
      advance st;
      Ast.Int n
  | Token.TIdent name ->
      advance st;
      if is_constructor_name name then
        if matches_lparen (current_kind st) then
          Ast.Constr (name, parse_parenthesized_args st)
        else Ast.Constr (name, [])
      else Ast.Var name
  | Token.TSucc -> parse_succ_expr st
  | Token.TLParen ->
      advance st;
      let expr = parse_expr st in
      expect st (function Token.TRParen -> true | _ -> false) "expected ')'";
      expr
  | _ -> Ravel_error.parse_error (current_pos st) "expected expression"

let rec parse_nonempty_patterns st =
  let rec loop acc =
    let pattern = parse_pattern st in
    match current_kind st with
    | Token.TComma ->
        advance st;
        loop (pattern :: acc)
    | Token.TRParen -> List.rev (pattern :: acc)
    | _ ->
        Ravel_error.parse_error (current_pos st)
          "expected ',' or ')' in function pattern list"
  in
  loop []

and parse_pattern_application_after_name st name =
  if name = "_" then
    Ravel_error.parse_error (current_pos st)
      "wildcard '_' cannot be applied as a constructor pattern";
  expect st matches_lparen "expected '(' after constructor name in pattern";
  let args =
    match current_kind st with
    | Token.TRParen -> []
    | _ ->
        let rec loop acc =
          let pattern = parse_pattern st in
          match current_kind st with
          | Token.TComma ->
              advance st;
              loop (pattern :: acc)
          | Token.TRParen -> List.rev (pattern :: acc)
          | _ ->
              Ravel_error.parse_error (current_pos st)
                "expected ',' or ')' in constructor pattern"
        in
        loop []
  in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after constructor pattern";
  Ast.PConstr (name, args)

and parse_succ_pattern st =
  expect st (function Token.TSucc -> true | _ -> false) "expected 'succ'";
  expect st (function Token.TLParen -> true | _ -> false)
    "expected '(' after 'succ' in pattern";
  let pattern = parse_pattern st in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after succ(...) pattern";
  match pattern with
  | Ast.PVar name -> Ast.PSucc name
  | _ -> Ast.PConstr ("succ", [ pattern ])

and parse_pattern st =
  match current_kind st with
  | Token.TInt 0 ->
      advance st;
      Ast.PZero
  | Token.TInt _ ->
      Ravel_error.parse_error (current_pos st)
        "only 0 is allowed as a literal pattern; use succ(...) or a constructor pattern"
  | Token.TSucc -> parse_succ_pattern st
  | Token.TIdent name ->
      advance st;
      if matches_lparen (current_kind st) then parse_pattern_application_after_name st name
      else if name = "_" then Ast.PWildcard
      else if is_constructor_name name then Ast.PConstr (name, [])
      else Ast.PVar name
  | _ ->
      Ravel_error.parse_error (current_pos st)
        "expected a pattern (0, succ(...), constructor, variable, or _)"

let parse_type_definition st =
  expect st (function Token.TData -> true | _ -> false) "expected 'data'";
  let type_name = expect_constructor_name st "type name" in
  expect st (function Token.TEqual -> true | _ -> false)
    "expected '=' after type name";
  let first = expect_constructor_name st "constructor name" in
  let rec parse_constructors acc =
    match current_kind st with
    | Token.TPipe ->
        advance st;
        let constructor = expect_constructor_name st "constructor name" in
        parse_constructors (constructor :: acc)
    | _ -> List.rev acc
  in
  { Ast.type_name; constructors = parse_constructors [ first ] }

let parse_definition st =
  expect st (function Token.TDef -> true | _ -> false) "expected 'def'";
  let name = expect_ident st "expected function name after 'def'" in
  expect st (function Token.TLParen -> true | _ -> false)
    "expected '(' after function name";
  let patterns =
    match current_kind st with
    | Token.TRParen ->
        Ravel_error.parse_error (current_pos st)
          "function definitions must have at least one pattern"
    | _ -> parse_nonempty_patterns st
  in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after function patterns";
  expect st (function Token.TEqual -> true | _ -> false)
    "expected '=' after function head";
  let body = parse_expr st in
  { Ast.name; patterns; body }

let parse source =
  let tokens = Lexer.tokenize source |> Array.of_list in
  let st = { tokens; index = 0 } in
  let rec parse_type_defs acc =
    match current_kind st with
    | Token.TData ->
        let def = parse_type_definition st in
        parse_type_defs (def :: acc)
    | _ -> List.rev acc
  in
  let type_definitions = parse_type_defs [] in
  let rec parse_defs acc =
    match current_kind st with
    | Token.TDef ->
        let def = parse_definition st in
        parse_defs (def :: acc)
    | _ -> List.rev acc
  in
  let definitions = parse_defs [] in
  let main = parse_expr st in
  match current_kind st with
  | Token.TEOF -> { Ast.type_definitions; definitions; main }
  | _ -> Ravel_error.parse_error (current_pos st) "unexpected trailing input"
