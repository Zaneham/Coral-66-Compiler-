(** CORAL 66 Compiler - Main Entry Point *)

open Coral66

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    Ok ast
  with
  | Lexer.Lexer_error (msg, line, col) ->
    close_in ic;
    Error (Printf.sprintf "%s:%d:%d: Lexer error: %s" filename line col msg)
  | Parser.Error ->
    close_in ic;
    let pos = lexbuf.Lexing.lex_curr_p in
    Error (Printf.sprintf "%s:%d:%d: Syntax error"
      pos.Lexing.pos_fname pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

let print_ast ast =
  print_endline (Ast.show_program ast)

let () =
  let usage = "coral66c [options] <file.cor>" in
  let input_file = ref "" in
  let dump_ast = ref false in
  let specs = [
    ("--ast", Arg.Set dump_ast, "Dump the AST");
  ] in
  Arg.parse specs (fun f -> input_file := f) usage;

  if !input_file = "" then begin
    prerr_endline "Error: No input file specified";
    prerr_endline usage;
    exit 1
  end;

  match parse_file !input_file with
  | Ok ast ->
    if !dump_ast then print_ast ast;
    print_endline "Compilation successful."
  | Error msg ->
    prerr_endline msg;
    exit 1
