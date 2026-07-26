type kind =
  | TInt of int
  | TIdent of string
  | TData
  | TDef
  | TFun
  | TLet
  | TIn
  | TDup
  | TAs
  | TDrop
  | TSucc
  | TLParen
  | TRParen
  | TComma
  | TEqual
  | TPipe
  | TEOF

type t = {
  kind : kind;
  start_pos : Ravel_error.position;
  end_pos : Ravel_error.position;
}
