%{
(** CORAL 66 Parser
    Based on the Official Definition of CORAL 66 (HMSO 1970) *)

open Ast

let mk_loc startpos _endpos = {
  file = startpos.Lexing.pos_fname;
  line = startpos.Lexing.pos_lnum;
  col = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
}
%}

(* Tokens *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> IDENT
%token <string> STRING_LIT
%token <string> OCTAL_LIT

(* Keywords *)
%token BEGIN END
%token IF THEN ELSE
%token FOR DO WHILE STEP UNTIL
%token GOTO
%token PROCEDURE RECURSIVE ANSWER
%token FLOATING FIXED INTEGER UNSIGNED BITS LABEL
%token ARRAY TABLE SWITCH OVERLAY WITH PRESET
%token VALUE LOCATION
%token COMMON LIBRARY EXTERNAL ABSOLUTE
%token AND OR DIFFER UNION MASK NOT ABS
%token OCTAL LITERAL
%token COMMENT
%token DEFINE DELETE
%token CODE

(* Operators and punctuation *)
%token ASSIGN    (* := *)
%token PLUS MINUS STAR SLASH
%token LT LE EQ GE GT NE
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON COLON

%token EOF

(* Precedence - lowest to highest *)
%left OR
%left AND
%left DIFFER UNION MASK
%nonassoc LT LE EQ GE GT NE
%left PLUS MINUS
%left STAR SLASH
%right NOT ABS
%nonassoc UMINUS

%start <Ast.program> program

%%

program:
  | BEGIN ds=decl_list ss=stmt_list END EOF
    { { prog_decls = ds; prog_stmts = ss; prog_loc = mk_loc $startpos $endpos } }
  | BEGIN ss=stmt_list END EOF
    { { prog_decls = []; prog_stmts = ss; prog_loc = mk_loc $startpos $endpos } }
  ;

decl_list:
  | d=decl SEMICOLON ds=decl_list { d :: ds }
  | d=decl SEMICOLON { [d] }
  ;

decl:
  | INTEGER ids=ident_list preset=preceded(ASSIGN, expr)?
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DVar (ids, TInteger, preset)) }
  | FLOATING ids=ident_list preset=preceded(ASSIGN, expr)?
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DVar (ids, TFloating, preset)) }
  | FIXED LPAREN tb=INT_LIT COMMA fb=INT_LIT RPAREN ids=ident_list preset=preceded(ASSIGN, expr)?
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DVar (ids, TFixed (tb, fb), preset)) }
  | UNSIGNED LPAREN b=INT_LIT RPAREN ids=ident_list preset=preceded(ASSIGN, expr)?
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DVar (ids, TUnsigned b, preset)) }
  | t=base_type ARRAY name=IDENT LBRACKET bounds=bounds_list RBRACKET
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DArray (name, t, bounds, None)) }
  | TABLE name=IDENT LBRACKET w=INT_LIT COMMA l=INT_LIT RBRACKET LBRACKET fs=table_fields RBRACKET
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DTable (name, { table_width = w; table_length = l; table_fields = fs })) }
  | SWITCH name=IDENT ASSIGN labels=ident_list
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DSwitch (name, labels)) }
  | ret=base_type? RECURSIVE? PROCEDURE name=IDENT params=procedure_params? SEMICOLON body=proc_body
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DProcedure {
        proc_name = name;
        proc_return = ret;
        proc_params = Option.value ~default:[] params;
        proc_recursive = $2 <> None;
        proc_body = body;
      }) }
  | EXTERNAL name=IDENT ret=preceded(COLON, base_type)?
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DExternal (name, ret)) }
  | COMMON ids=ident_list
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DCommon ids) }
  | LIBRARY ids=ident_list
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DLibrary ids) }
  | ABSOLUTE name=IDENT addr=INT_LIT
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DAbsolute (name, addr)) }
  | DEFINE name=IDENT s=STRING_LIT
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DDefine (name, s)) }
  | DELETE name=IDENT
    { mk_decl ~loc:(mk_loc $startpos $endpos) (DDelete name) }
  ;

base_type:
  | INTEGER { TInteger }
  | FLOATING { TFloating }
  | FIXED LPAREN tb=INT_LIT COMMA fb=INT_LIT RPAREN { TFixed (tb, fb) }
  | UNSIGNED LPAREN b=INT_LIT RPAREN { TUnsigned b }
  ;

ident_list:
  | id=IDENT { [id] }
  | id=IDENT COMMA ids=ident_list { id :: ids }
  ;

bounds_list:
  | b=bounds { [b] }
  | b=bounds COMMA bs=bounds_list { b :: bs }
  ;

bounds:
  | lo=expr COLON hi=expr { { lower = lo; upper = hi } }
  ;

table_fields:
  | f=table_field { [f] }
  | f=table_field SEMICOLON fs=table_fields { f :: fs }
  ;

table_field:
  | name=IDENT t=base_type wp=INT_LIT bp=preceded(COMMA, INT_LIT)?
    { { field_name = name; field_type = t; field_wordpos = wp; field_bitpos = bp } }
  ;

procedure_params:
  | LPAREN ps=param_specs RPAREN { ps }
  ;

param_specs:
  | p=param_spec { p }
  | p=param_spec SEMICOLON ps=param_specs { p @ ps }
  ;

param_spec:
  | VALUE t=base_type COLON ids=ident_list
    { List.map (fun name -> { param_name = name; param_type = t; param_mode = Value }) ids }
  | LOCATION t=base_type COLON ids=ident_list
    { List.map (fun name -> { param_name = name; param_type = t; param_mode = Location }) ids }
  ;

proc_body:
  | BEGIN _ds=decl_list ss=stmt_list END { ss }
  | BEGIN ss=stmt_list END { ss }
  | s=stmt { [s] }
  ;

stmt_list:
  | s=stmt { [s] }
  | s=stmt SEMICOLON { [s] }
  | s=stmt SEMICOLON ss=stmt_list { s :: ss }
  ;

stmt:
  | name=IDENT ASSIGN e=expr
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SAssign (name, e)) }
  | name=IDENT LBRACKET idxs=expr_list RBRACKET ASSIGN e=expr
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SSubscriptAssign (name, idxs, e)) }
  | name=IDENT LPAREN args=expr_list RPAREN
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SCall (name, args)) }
  | name=IDENT
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SCall (name, [])) }
  | IF cond=expr THEN t=stmt_block
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SIf (cond, t, None)) }
  | IF cond=expr THEN t=stmt_block ELSE f=stmt_block
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SIf (cond, t, Some f)) }
  | FOR v=IDENT ASSIGN init=expr step=preceded(STEP, expr)? UNTIL until=expr DO body=stmt_block
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SFor ({ for_var = v; for_init = init; for_step = step; for_until = until }, body)) }
  | WHILE cond=expr DO body=stmt_block
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SWhile (cond, body)) }
  | ANSWER e=expr
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SAnswer e) }
  | GOTO lbl=IDENT
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SGoto lbl) }
  | lbl=IDENT COLON
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SLabel lbl) }
  | BEGIN ds=decl_list ss=stmt_list END
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SBlock (ds, ss)) }
  | BEGIN ss=stmt_list END
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SBlock ([], ss)) }
  | CODE s=STRING_LIT
    { mk_stmt ~loc:(mk_loc $startpos $endpos) (SCode s) }
  ;

stmt_block:
  | s=stmt { [s] }
  | BEGIN ss=stmt_list END { ss }
  ;

expr_list:
  | e=expr { [e] }
  | e=expr COMMA es=expr_list { e :: es }
  | (* empty *) { [] }
  ;

expr:
  | i=INT_LIT
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EInt i) }
  | f=FLOAT_LIT
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EFloat f) }
  | o=OCTAL_LIT
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EOctal o) }
  | s=STRING_LIT
    { mk_expr ~loc:(mk_loc $startpos $endpos) (ELiteral s) }
  | name=IDENT
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EIdent name) }
  | name=IDENT LPAREN args=expr_list RPAREN
    { mk_expr ~loc:(mk_loc $startpos $endpos) (ECall (name, args)) }
  | name=IDENT LBRACKET idxs=expr_list RBRACKET
    { mk_expr ~loc:(mk_loc $startpos $endpos) (ESubscript (name, idxs)) }
  | LPAREN e=expr RPAREN
    { e }
  (* Type conversions *)
  | INTEGER LPAREN e=expr RPAREN
    { mk_expr ~loc:(mk_loc $startpos $endpos) (ETypeConv (TInteger, e)) }
  | FLOATING LPAREN e=expr RPAREN
    { mk_expr ~loc:(mk_loc $startpos $endpos) (ETypeConv (TFloating, e)) }
  (* Arithmetic *)
  | e1=expr PLUS e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Add, e1, e2)) }
  | e1=expr MINUS e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Sub, e1, e2)) }
  | e1=expr STAR e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Mul, e1, e2)) }
  | e1=expr SLASH e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Div, e1, e2)) }
  | MINUS e=expr %prec UMINUS
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EUnop (Neg, e)) }
  (* Comparison *)
  | e1=expr LT e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Lt, e1, e2)) }
  | e1=expr LE e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Le, e1, e2)) }
  | e1=expr EQ e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Eq, e1, e2)) }
  | e1=expr GE e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Ge, e1, e2)) }
  | e1=expr GT e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Gt, e1, e2)) }
  | e1=expr NE e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Ne, e1, e2)) }
  (* Word logic *)
  | e1=expr AND e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (And, e1, e2)) }
  | e1=expr OR e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Or, e1, e2)) }
  | e1=expr DIFFER e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Differ, e1, e2)) }
  | e1=expr UNION e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Union, e1, e2)) }
  | e1=expr MASK e2=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EBinop (Mask, e1, e2)) }
  (* Unary *)
  | NOT e=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EUnop (Not, e)) }
  | ABS e=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EUnop (Abs, e)) }
  (* Conditional *)
  | IF cond=expr THEN t=expr ELSE f=expr
    { mk_expr ~loc:(mk_loc $startpos $endpos) (EIf (cond, t, f)) }
  ;
