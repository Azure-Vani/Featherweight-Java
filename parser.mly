%{
    open Jtype
%}

%token <int> INT
%token <string> IDEN
%token CLASS
%token RETURN
%token EXTENDS
%token SUPER
%token NEW

%token PLUS
%token MINUS
%token TIMES

%token L_BRACE
%token R_BRACE
%token L_PARENTHESIS
%token R_PARENTHESIS
%token COMMA
%token SEMICOLON
%token DOT
%token EQ
%token EOF

%left PLUS MINUS
%left TIMES
%nonassoc INVOKE
%nonassoc CAST
%start <Jtype.adt> prog
%%

prog:
    | c = class_list; t = expr; EOF { {classes = c; term = t} } 
    ;

class_list: 
    | c = list(j_class) {c}
    ;

j_class:
    CLASS; n=IDEN; EXTENDS; s=IDEN; L_BRACE; b = class_body; R_BRACE 
    { Jtype.{name = n; super = s; items = b} }
    ;

class_body:
    l = list(class_body_item); {l}
    ;

declar:
    | t = IDEN; x = IDEN { {ty = t; iden = x} }
    ;

declar_params:
    l = separated_list(COMMA, declar) {l}
    ;

invok_params:
    l = separated_list(COMMA, expr) {l}
    ;

class_body_item:
    | d = declar; SEMICOLON 
        { `Field d }

    | IDEN; L_PARENTHESIS; l = declar_params; R_PARENTHESIS; 
        L_BRACE; s = list(statement); R_BRACE 
        {flush stdout; `Cons {params = l; statements = s} }

    | t = IDEN; n = IDEN; L_PARENTHESIS; l = declar_params; R_PARENTHESIS;
        L_BRACE; RETURN; e = expr; SEMICOLON; R_BRACE; 
        { `Method {name = n; return_type = t; params = l; return_stat = e} }

    ;

statement:
    | SUPER; L_PARENTHESIS; l = invok_params; R_PARENTHESIS; SEMICOLON 
        { `Super l }
    
    | IDEN; DOT; v = IDEN; EQ; e = expr;  SEMICOLON
        { `Assign (v, e) }
    ;

expr:
    | L_PARENTHESIS; e = expr; R_PARENTHESIS  {e}

    | e1 = expr; PLUS; e2 = expr {Plus (e1, e2)}

    | e1 = expr; MINUS; e2 = expr {Minus (e1, e2)}

    | e1 = expr; TIMES; e2 = expr {Times (e1, e2)} 

    | L_PARENTHESIS; v = IDEN; R_PARENTHESIS; e = expr; {Cast (v, e)} %prec CAST

    | NEW; v = IDEN; L_PARENTHESIS; l = invok_params; R_PARENTHESIS {New (v, l)}

    | e = expr; DOT; f = IDEN; L_PARENTHESIS; l = invok_params; R_PARENTHESIS {Invoke (e, f, l)} %prec INVOKE

    | e = expr; DOT; x = IDEN; {Access (e, x)}

    | x = INT {Value (Primary x)}

    | x = IDEN {Value (Variable x)}

