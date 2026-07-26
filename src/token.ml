type kind =
  | TInt of int
  | TIdent of string
  | TDef
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
  | TEOF

type t = {
  kind : kind;
  start_pos : Ravel_error.position;
  end_pos : Ravel_error.position;
}
