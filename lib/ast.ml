(** CORAL 66 Abstract Syntax Tree
    Based on the Official Definition of CORAL 66 (HMSO 1970)
    Reference: XGC CORAL 66 Language Reference Manual (Crown Copyright 1970) *)

(** Source location for error reporting *)
type loc = {
  file: string;
  line: int;
  col: int;
}
[@@deriving show, eq]

let dummy_loc = { file = "<unknown>"; line = 0; col = 0 }

(** CORAL 66 basic types *)
type coral_type =
  | TInteger
  | TFloating
  | TFixed of int * int             (** FIXED(totalbits, fractionbits) *)
  | TUnsigned of int                (** UNSIGNED(bits) *)
  | TBits of int                    (** BITS(n) *)
  | TLabel
  | TArray of coral_type * bounds list
  | TTable of table_field list
  | TProcedure of coral_type option * param list
[@@deriving show, eq]

and bounds = {
  lower: expr;
  upper: expr;
}
[@@deriving show, eq]

and table_field = {
  field_name: string;
  field_type: coral_type;
  field_wordpos: int;
  field_bitpos: int option;         (** Optional bit position within word *)
}
[@@deriving show, eq]

and param = {
  param_name: string;
  param_type: coral_type;
  param_mode: param_mode;
}
[@@deriving show, eq]

and param_mode =
  | Value                           (** VALUE - pass by value *)
  | Location                        (** LOCATION - pass by reference *)
[@@deriving show, eq]

(** Expressions *)
and expr = {
  expr_desc: expr_desc;
  expr_loc: loc;
}
[@@deriving show, eq]

and expr_desc =
  | EInt of int
  | EFloat of float
  | EOctal of string                (** OCTAL literal *)
  | ELiteral of string              (** LITERAL string *)
  | EIdent of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECall of string * expr list
  | ESubscript of string * expr list      (** Array/table subscript *)
  | EField of expr * string               (** Table field access *)
  | EIf of expr * expr * expr             (** Conditional expression *)
  | ETypeConv of coral_type * expr        (** Type conversion: FLOATING(x) *)
[@@deriving show, eq]

and binop =
  (* Arithmetic *)
  | Add | Sub | Mul | Div
  (* Comparison *)
  | Eq | Ne | Lt | Le | Gt | Ge
  (* Word logic (bitwise operations) *)
  | And | Or | Differ | Union | Mask
[@@deriving show, eq]

and unop =
  | Neg | Not | Abs
[@@deriving show, eq]

(** Statements *)
type stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: loc;
}
[@@deriving show, eq]

and stmt_desc =
  | SAssign of string * expr                    (** x := e *)
  | SSubscriptAssign of string * expr list * expr  (** a[i] := e *)
  | SCall of string * expr list                 (** procedure call *)
  | SIf of expr * stmt list * stmt list option  (** IF/THEN/ELSE *)
  | SFor of for_clause * stmt list              (** FOR ... DO *)
  | SWhile of expr * stmt list                  (** WHILE ... DO *)
  | SAnswer of expr                             (** ANSWER e (return) *)
  | SGoto of string                             (** GOTO label *)
  | SLabel of string                            (** label: *)
  | SSwitch of string * expr                    (** switch assignment *)
  | SBlock of decl list * stmt list             (** BEGIN ... END *)
  | SComment of string                          (** COMMENT ...; *)
  | SCode of string                             (** CODE inline assembly *)
[@@deriving show, eq]

and for_clause = {
  for_var: string;
  for_init: expr;
  for_step: expr option;            (** STEP value, defaults to 1 *)
  for_until: expr;
}
[@@deriving show, eq]

(** Declarations *)
and decl = {
  decl_desc: decl_desc;
  decl_loc: loc;
}
[@@deriving show, eq]

and decl_desc =
  | DVar of string list * coral_type * expr option
      (** Variable declaration: INTEGER x, y := 0 *)
  | DArray of string * coral_type * bounds list * expr list option
      (** Array declaration with optional preset values *)
  | DTable of string * table_def
      (** Table (record) declaration *)
  | DSwitch of string * string list
      (** Switch declaration: SWITCH name := label1, label2 *)
  | DOverlay of string * expr list option * decl
      (** OVERLAY base WITH datadec *)
  | DProcedure of proc_def
      (** Procedure declaration *)
  | DCommon of string list
      (** COMMON block names *)
  | DLibrary of string list
      (** LIBRARY references *)
  | DExternal of string * coral_type option
      (** EXTERNAL procedure reference *)
  | DAbsolute of string * int
      (** ABSOLUTE address binding *)
  | DDefine of string * string
      (** DEFINE macro *)
  | DDelete of string
      (** DELETE macro *)
[@@deriving show, eq]

and table_def = {
  table_width: int;                 (** Words per entry *)
  table_length: int;                (** Number of entries *)
  table_fields: table_field list;
}
[@@deriving show, eq]

and proc_def = {
  proc_name: string;
  proc_return: coral_type option;   (** None for void procedures *)
  proc_params: param list;
  proc_recursive: bool;
  proc_body: stmt list;
}
[@@deriving show, eq]

(** Top-level program *)
type program = {
  prog_decls: decl list;
  prog_stmts: stmt list;
  prog_loc: loc;
}
[@@deriving show, eq]

(** Helper constructors *)
let mk_expr ?(loc=dummy_loc) desc = { expr_desc = desc; expr_loc = loc }
let mk_stmt ?(loc=dummy_loc) desc = { stmt_desc = desc; stmt_loc = loc }
let mk_decl ?(loc=dummy_loc) desc = { decl_desc = desc; decl_loc = loc }

(** Type helpers *)
let integer = TInteger
let floating = TFloating
let fixed bits frac = TFixed (bits, frac)
let unsigned bits = TUnsigned bits
let bits n = TBits n
let label = TLabel
