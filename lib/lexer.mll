{
(** CORAL 66 Lexer
    Based on the Official Definition of CORAL 66 (HMSO 1970) *)

open Parser

exception Lexer_error of string * int * int

let keyword_table = Hashtbl.create 53
let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) [
  (* Block structure *)
  "BEGIN", BEGIN;
  "END", END;
  (* Control flow *)
  "IF", IF;
  "THEN", THEN;
  "ELSE", ELSE;
  "FOR", FOR;
  "DO", DO;
  "WHILE", WHILE;
  "STEP", STEP;
  "UNTIL", UNTIL;
  "GOTO", GOTO;
  (* Procedures *)
  "PROCEDURE", PROCEDURE;
  "RECURSIVE", RECURSIVE;
  "ANSWER", ANSWER;
  (* Data types *)
  "FLOATING", FLOATING;
  "FIXED", FIXED;
  "INTEGER", INTEGER;
  "UNSIGNED", UNSIGNED;
  "BITS", BITS;
  "LABEL", LABEL;
  (* Declarations *)
  "ARRAY", ARRAY;
  "TABLE", TABLE;
  "SWITCH", SWITCH;
  "OVERLAY", OVERLAY;
  "WITH", WITH;
  "PRESET", PRESET;
  (* Parameter modes *)
  "VALUE", VALUE;
  "LOCATION", LOCATION;
  (* Communicators *)
  "COMMON", COMMON;
  "LIBRARY", LIBRARY;
  "EXTERNAL", EXTERNAL;
  "ABSOLUTE", ABSOLUTE;
  (* Word logic operators *)
  "AND", AND;
  "OR", OR;
  "DIFFER", DIFFER;
  "UNION", UNION;
  "MASK", MASK;
  "NOT", NOT;
  "ABS", ABS;
  (* Literals *)
  "OCTAL", OCTAL;
  "LITERAL", LITERAL;
  (* Comments *)
  "COMMENT", COMMENT;
  (* Macros *)
  "DEFINE", DEFINE;
  "DELETE", DELETE;
  (* Inline assembly *)
  "CODE", CODE;
]

let newline lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let whitespace = [' ' '\t']

rule token = parse
  (* Whitespace *)
  | whitespace+    { token lexbuf }
  | '\n'           { newline lexbuf; token lexbuf }
  | '\r' '\n'      { newline lexbuf; token lexbuf }
  | '\r'           { newline lexbuf; token lexbuf }

  (* Comments: COMMENT ... ; *)
  | "COMMENT"      { comment lexbuf }

  (* Two-character operators *)
  | ":="           { ASSIGN }
  | "<="           { LE }
  | ">="           { GE }
  | "<>"           { NE }

  (* Single-character tokens *)
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { STAR }
  | '/'            { SLASH }
  | '<'            { LT }
  | '>'            { GT }
  | '='            { EQ }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { LBRACKET }
  | ']'            { RBRACKET }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | ':'            { COLON }

  (* Floating point numbers *)
  | digit+ '.' digit* (['e' 'E'] ['+' '-']? digit+)?
                   { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }
  | digit+ ['e' 'E'] ['+' '-']? digit+
                   { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }

  (* Integer numbers *)
  | digit+         { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }

  (* Octal literals: start with # *)
  | '#' ['0'-'7']+ { OCTAL_LIT (Lexing.lexeme lexbuf) }

  (* Identifiers and keywords - CORAL 66 is case-insensitive, we uppercase *)
  | alpha alphanum*
    { let s = String.uppercase_ascii (Lexing.lexeme lexbuf) in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT (String.lowercase_ascii s) }

  (* String literals in single quotes *)
  | '\'' [^ '\'']* '\''
    { let s = Lexing.lexeme lexbuf in
      STRING_LIT (String.sub s 1 (String.length s - 2)) }

  (* End of file *)
  | eof            { EOF }

  (* Error *)
  | _
    { let pos = lexbuf.Lexing.lex_curr_p in
      raise (Lexer_error (
        Printf.sprintf "Unexpected character: %s" (Lexing.lexeme lexbuf),
        pos.Lexing.pos_lnum,
        pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) }

and comment = parse
  | ';'            { token lexbuf }
  | '\n'           { newline lexbuf; comment lexbuf }
  | eof            { raise (Lexer_error ("Unterminated comment", 0, 0)) }
  | _              { comment lexbuf }
