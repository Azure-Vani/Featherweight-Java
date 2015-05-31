(* File lexer.mll *)
{
    open Lexing
    open Parser
    open Core.Caml
    open Core.Std
}

let digit = ['0'-'9']+
let identifier = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* 

let space = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | "class" {printf "<class>\n"; flush stdout; CLASS }
  | "return" {printf "<return>\n"; flush stdout; RETURN }
  | "extends" {printf "<extends>\n"; flush stdout; EXTENDS }
  | "super" {printf "<super>\n"; flush stdout; SUPER }
  | "new" {printf "<new>\n"; flush stdout; NEW }

  | '+' { printf "+\n"; flush stdout; PLUS }
  | '-' { printf "-\n"; flush stdout; MINUS }
  | '*' { printf "*\n"; flush stdout; TIMES }
  | '{' { printf "{\n"; flush stdout; L_BRACE }
  | '}' { printf "}\n"; flush stdout; R_BRACE }
  | '(' { printf "(\n"; flush stdout; L_PARENTHESIS }
  | ')' { printf ")\n"; flush stdout; R_PARENTHESIS }
  | ',' { printf ",\n"; flush stdout; COMMA }
  | ';' { printf ";\n"; flush stdout; SEMICOLON }
  | '.' { printf ".\n"; flush stdout; DOT }
  | '=' { printf "=\n"; flush stdout; EQ }

  | digit { printf "%s\n" @@ lexeme lexbuf; flush stdout; INT(int_of_string @@ lexeme lexbuf) }
  | identifier { printf "%s\n" @@ lexeme lexbuf; flush stdout; IDEN(lexeme lexbuf) }

  | space { (* printf "space\n"; *) flush stdout; token lexbuf }
  | newline { (* printf "newline\n"; *) flush stdout; token lexbuf }

  | eof            { EOF }
