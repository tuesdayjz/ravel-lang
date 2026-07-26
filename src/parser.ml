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

and parse_call_after_name st name =
  expect st (function Token.TLParen -> true | _ -> false)
    "expected '(' after function name";
  let args =
    match current_kind st with
    | Token.TRParen ->
        Ravel_error.parse_error (current_pos st)
          "function calls must have at least one argument"
    | _ -> parse_nonempty_args st
  in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after argument list";
  Ast.Call (name, args)

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
  | _ -> parse_atom st

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

and parse_atom st =
  match current_kind st with
  | Token.TInt n ->
      advance st;
      Ast.Int n
  | Token.TIdent name ->
      advance st;
      if matches_lparen (current_kind st) then parse_call_after_name st name
      else Ast.Var name
  | Token.TSucc -> parse_succ_expr st
  | Token.TLParen ->
      advance st;
      let expr = parse_expr st in
      expect st (function Token.TRParen -> true | _ -> false) "expected ')'";
      expr
  | _ -> Ravel_error.parse_error (current_pos st) "expected expression"

and matches_lparen = function Token.TLParen -> true | _ -> false

let parse_pattern st =
  match current_kind st with
  | Token.TInt 0 ->
      advance st;
      Ast.PZero
  | Token.TInt _ ->
      Ravel_error.parse_error (current_pos st)
        "only 0 is allowed as a literal pattern; use succ(x) for successors"
  | Token.TSucc ->
      advance st;
      expect st (function Token.TLParen -> true | _ -> false)
        "expected '(' after 'succ' in pattern";
      let name = expect_ident st "expected identifier inside succ(...) pattern" in
      expect st (function Token.TRParen -> true | _ -> false)
        "expected ')' after succ(...) pattern";
      Ast.PSucc name
  | Token.TIdent name ->
      advance st;
      Ast.PVar name
  | _ ->
      Ravel_error.parse_error (current_pos st)
        "expected a pattern (0, succ(x), or variable)"

let parse_definition st =
  expect st (function Token.TDef -> true | _ -> false) "expected 'def'";
  let name = expect_ident st "expected function name after 'def'" in
  expect st (function Token.TLParen -> true | _ -> false)
    "expected '(' after function name";
  let pattern = parse_pattern st in
  let rec parse_rest_params acc =
    match current_kind st with
    | Token.TComma ->
        advance st;
        let param = expect_ident st "expected parameter name" in
        parse_rest_params (param :: acc)
    | Token.TRParen -> List.rev acc
    | _ ->
        Ravel_error.parse_error (current_pos st)
          "expected ',' or ')' in function parameter list"
  in
  let params = parse_rest_params [] in
  expect st (function Token.TRParen -> true | _ -> false)
    "expected ')' after function parameters";
  expect st (function Token.TEqual -> true | _ -> false)
    "expected '=' after function head";
  let body = parse_expr st in
  { Ast.name; pattern; params; body }

let parse source =
  let tokens = Lexer.tokenize source |> Array.of_list in
  let st = { tokens; index = 0 } in
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
  | Token.TEOF -> { Ast.definitions; main }
  | _ -> Ravel_error.parse_error (current_pos st) "unexpected trailing input"
