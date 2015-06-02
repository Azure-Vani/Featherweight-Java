(* File lexer.mll *)
{
    open Lexing
    open Parser
    open Core.Caml
    open Core.Std
    open Exceptions

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p
        in lexbuf.lex_curr_p <- {
            pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1;
        }

    let get_line_number lexbuf = lexbuf.lex_curr_p.pos_lnum

    let get_col_number lexbuf = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol - (String.length @@ lexeme lexbuf) + 1

    let debug_print str lexbuf = ()
        (* only enable in debug
        printf "@(%d, %d) %s\n" (get_line_number lexbuf) (get_col_number lexbuf) str;
        flush stdout
        *)
}

let digit = ['0'-'9']+
let identifier = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

let space = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | "class" {
      debug_print "<class>" lexbuf;
      CLASS
  }

  | "return" {
      debug_print "<return>" lexbuf;
      RETURN
  }

  | "extends" {
      debug_print "<extends>\n" lexbuf;
      EXTENDS
  }

  | "super" {
      debug_print "<super>" lexbuf;
      SUPER
  }

  | "new" {
      debug_print "<new>" lexbuf;
      NEW
  }

  | '+' {
      debug_print "+" lexbuf;
      PLUS
  }

  | '-' {
      debug_print "-" lexbuf;
      MINUS
  }

  | '*' {
      debug_print "*" lexbuf;
      TIMES
  }

  | '{' {
      debug_print "{" lexbuf;
      L_BRACE
  }

  | '}' {
      debug_print "}" lexbuf;
      R_BRACE
  }

  | '(' {
      debug_print "(" lexbuf;
      L_PARENTHESIS
  }

  | ')' {
      debug_print ")" lexbuf;
      R_PARENTHESIS
  }

  | ',' {
      debug_print "," lexbuf;
      COMMA
  }

  | ';' {
      debug_print ";\n" lexbuf;
      SEMICOLON
  }

  | '.' {
      debug_print ".\n" lexbuf;
      DOT
  }

  | '=' {
      debug_print "=\n" lexbuf;
      EQ
  }

  | digit {
      debug_print (lexeme lexbuf) lexbuf;
      INT(int_of_string @@ lexeme lexbuf)
  }

  | identifier {
      debug_print (lexeme lexbuf) lexbuf;
      IDEN(lexeme lexbuf)
  }

  | space {
      token lexbuf
  }

  | newline {
      next_line lexbuf;
      token lexbuf
  }

  | eof            { EOF }

  |  _ {raise (Token_exn (get_line_number lexbuf, get_col_number lexbuf, lexeme lexbuf))}
