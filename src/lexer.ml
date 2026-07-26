let is_digit c = c >= '0' && c <= '9'

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

let keyword_or_ident = function
  | "def" -> Token.TDef
  | "let" -> Token.TLet
  | "in" -> Token.TIn
  | "dup" -> Token.TDup
  | "as" -> Token.TAs
  | "drop" -> Token.TDrop
  | "succ" -> Token.TSucc
  | name -> Token.TIdent name

let pos line column : Ravel_error.position = { line; column }

let tokenize source =
  let len = String.length source in
  let rec skip i line column =
    if i >= len then (i, line, column)
    else
      match source.[i] with
      | ' ' | '\t' | '\r' -> skip (i + 1) line (column + 1)
      | '\n' -> skip (i + 1) (line + 1) 1
      | '#' -> skip_comment (i + 1) line (column + 1)
      | _ -> (i, line, column)
  and skip_comment i line column =
    if i >= len then (i, line, column)
    else if source.[i] = '\n' then skip i line column
    else skip_comment (i + 1) line (column + 1)
  in
  let rec lex i line column acc =
    let i, line, column = skip i line column in
    if i >= len then
      List.rev
        ({
           Token.kind = Token.TEOF;
           start_pos = pos line column;
           end_pos = pos line column;
         }
        :: acc)
    else
      let start_pos = pos line column in
      match source.[i] with
      | '(' ->
          let tok =
            {
              Token.kind = Token.TLParen;
              start_pos;
              end_pos = pos line (column + 1);
            }
          in
          lex (i + 1) line (column + 1) (tok :: acc)
      | ')' ->
          let tok =
            {
              Token.kind = Token.TRParen;
              start_pos;
              end_pos = pos line (column + 1);
            }
          in
          lex (i + 1) line (column + 1) (tok :: acc)
      | ',' ->
          let tok =
            {
              Token.kind = Token.TComma;
              start_pos;
              end_pos = pos line (column + 1);
            }
          in
          lex (i + 1) line (column + 1) (tok :: acc)
      | '=' ->
          let tok =
            {
              Token.kind = Token.TEqual;
              start_pos;
              end_pos = pos line (column + 1);
            }
          in
          lex (i + 1) line (column + 1) (tok :: acc)
      | c when is_digit c ->
          let j = ref i in
          let col = ref column in
          while !j < len && is_digit source.[!j] do
            incr j;
            incr col
          done;
          let value = int_of_string (String.sub source i (!j - i)) in
          let tok =
            {
              Token.kind = Token.TInt value;
              start_pos;
              end_pos = pos line !col;
            }
          in
          lex !j line !col (tok :: acc)
      | c when is_ident_start c ->
          let j = ref i in
          let col = ref column in
          while !j < len && is_ident_continue source.[!j] do
            incr j;
            incr col
          done;
          let text = String.sub source i (!j - i) in
          let tok =
            {
              Token.kind = keyword_or_ident text;
              start_pos;
              end_pos = pos line !col;
            }
          in
          lex !j line !col (tok :: acc)
      | c ->
          Ravel_error.parse_error start_pos
            (Printf.sprintf "unexpected character %C" c)
  in
  lex 0 1 1 []
