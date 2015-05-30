(* File lexer.mll *)
{
    open Lexing
    open Parser
    open Core.Caml
}


let digit = ['0'-'9']+
let identifier = ['a'-'z''A'-'Z'_]['a'-'z''A'-'Z''0'-'9'_]+ 

let space = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | digit { DIGIT(int_of_string @@ lexeme lexbuf) }
  | identifier { STR(lexeme lexbuf) }

  | "class" { CLASS }
  | "return" { RETURN }
  | "this" { THIS }
  | "extends" { EXTENDS }
  | "super" { SUPER }
  | "new" { NEW }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '{' { L_BRACE }
  | '}' { R_BRACE }
  | '(' { L_PARENTHESIS }
  | ')' { R_PARENTHESIS }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '.' { DOT }
  | '=' { EQ }

  | space { token lexbuf }
  | newline { token lexbuf }

  | eof            { EOF }
